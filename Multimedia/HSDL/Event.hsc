{-# OPTIONS -fglasgow-exts #-}

module Multimedia.HSDL.Event(
  Event(..),
  EventFlag(..),
  AppStates(..),
  MouseButton(..),
  Keysym(..),
  EventFilter,

  pumpEvents,
  pushEvents,
  peekEvents,
  pollEvent,
  waitEvent,
  setEventFilter,

  -- sdlGetEventFilter,
  getEventState,
  setEventState,

  getKeyState,
  getModState,
  setModState,
  getKeyName,
  enableUNICODE,
  enableKeyRepeat,
  getMouseState,
  getRelativeMouseState,
  getAppState,
) where

import Foreign
import Foreign.C
import Multimedia.HSDL.Util
import Multimedia.HSDL.Keysym

data Event =
    ActiveEvent    { acGain  :: Bool, acState :: [AppStates] }
  | KeyboardEvent  { kbPress :: Bool, kbState :: Bool, kbKeysym :: Keysym}
  | MouseMotionEvent { mmState :: [MouseButton], mmPos :: Point, mmRel :: Point }
  | MouseButtonEvent { mbPress :: Bool, mbButton :: MouseButton, mbState :: Bool, mbPos :: Point }
  | JoyAxisEvent   { jaWhich :: Int,  jaAxis   :: Int, jaValue :: Int }
  | JoyBallEvent   { jbWhich :: Int,  jbBall   :: Int, jbRel   :: Point }
  | JoyHatEvent    { jhWhich :: Int,  jhHat    :: Int, jhValue :: Int }
  | JoyButtonEvent { jbType  :: Bool, jbWhich  :: Int, jbButton :: Int, jbState :: Bool }
  | ResizeEvent    { rsSize :: Size }
  | ExposeEvent
  | QuitEvent
  | UnknownEvent   { ukType :: Word8 }
--  | UserEvent  {} -- TODO : どう定義するか?
--  | SysWMEvent {}
  deriving (Eq,Show)

instance Storable Event where
  sizeOf    _ = 20 -- KeyboardEventが最大、20バイト
  alignment _ = 4

  peek p = do
    (t :: Word8) <- peekByteOff p 0
    case t of
       -- ACTIVEEVENT
       1 -> do
         (g :: Word8) <- peekByteOff p 1
         (s :: Word8) <- peekByteOff p 2
         return $ ActiveEvent (toBool g) (toFlags s)

       -- HSDL_KEYDOWN
       -- HSDL_KEYUP
       _ | t==2 || t==3 -> do
         (s  :: Word8)  <- peekByteOff p 2
         (sc :: Word8)  <- peekByteOff p 4
         (sy :: Int  )  <- peekByteOff p 8
         (mo :: Int  )  <- peekByteOff p 12
         (un :: Word16) <- peekByteOff p 16
         return $ KeyboardEvent (t==2) (toBool s) $ Keysym sc (toEnum sy) (toFlags mo) un

       -- HSDL_MOUSEMOTION
       4 -> do
         (s  :: Word8)  <- peekByteOff p 2
         (x  :: Word16) <- peekByteOff p 4
         (y  :: Word16) <- peekByteOff p 6
         (xr :: Int16)  <- peekByteOff p 8
         (yr :: Int16)  <- peekByteOff p 10
         return $ MouseMotionEvent (toFlags s) (Point (fromEnum x) (fromEnum y)) (Point (fromEnum xr) (fromEnum yr))

       -- MOUSEBUTTONDOWN
       -- MOUSEBUTTONUP
       _ | t==5 || t==6 -> do
         (b :: Word8)  <- peekByteOff p 2
         (s :: Word8)  <- peekByteOff p 3
         (x :: Word16) <- peekByteOff p 4
         (y :: Word16) <- peekByteOff p 6
         return $ MouseButtonEvent (t==5) ((toEnum.fromEnum) (b-1)) (toBool b) (Point (fromEnum x) (fromEnum y))

       -- JOYAXISMOTION
       7 -> do
         (w :: Word8)  <- peekByteOff p 1
         (a :: Word8)  <- peekByteOff p 2
         (v :: Word16) <- peekByteOff p 4
         return $ JoyAxisEvent (fromEnum w) (fromEnum a) (fromEnum v)

       -- JOYBALLMOTION
       8 -> do
         (w  :: Word8) <- peekByteOff p 1
         (b  :: Word8) <- peekByteOff p 2
         (xr :: Int16) <- peekByteOff p 4
         (yr :: Int16) <- peekByteOff p 6
         return $ JoyBallEvent (fromEnum w) (fromEnum b) (Point (fromEnum xr) (fromEnum yr))

       -- JOYHATMOTION
       9 -> do
         (w  :: Word8) <- peekByteOff p 1
         (h  :: Word8) <- peekByteOff p 2
         (v  :: Word8) <- peekByteOff p 3
         return $ JoyHatEvent (fromEnum w) (fromEnum h) (fromEnum v)

       -- JOYBUTTONDOWN
       -- JOYBUTTONUP
       _ | t==10 || t==11 -> do
         (w  :: Word8) <- peekByteOff p 1
         (b  :: Word8) <- peekByteOff p 2
         (s  :: Word8) <- peekByteOff p 3
         return $ JoyButtonEvent (t==10) (fromEnum w) (fromEnum b) (toBool s)

       -- QUIT
       12 -> return QuitEvent

       -- VIDEORESIZE
       16 -> do
         (w :: Int) <- peekByteOff p 4
         (h :: Int) <- peekByteOff p 8
         return $ ResizeEvent (Size w h)

       -- VIDEOEXPOSE
       17 -> return ExposeEvent

       -- SYSWMEVENT      13 -- not implemented
       -- EVENT_RESERVEDA 14
       -- EVENT_RESERVEDB 15
       -- USEREVENT       24-32
       _ -> return $ UnknownEvent t

  poke p (ActiveEvent { acGain = g, acState = ss }) = do
    pokeByteOff p 0 (1          :: Word8)
    pokeByteOff p 1 ((fromBool g) :: Word8)
    pokeByteOff p 2 ((fromFlags ss) :: Word8)

  poke p (KeyboardEvent { kbPress = t, kbState = s, kbKeysym =
         (Keysym { ksScancode = sc, ksSym = sy, ksMod = mo, ksUnicode = un })}) = do
    pokeByteOff p 0 ((if t then 2 else 3) :: Word8)
    pokeByteOff p 2 ((fromBool s)         :: Word8)
    pokeByteOff p 4 sc
    pokeByteOff p 8 (fromEnum sy)
    pokeByteOff p 12 ((fromFlags mo)       :: Int)
    pokeByteOff p 16 un

  poke p (MouseMotionEvent { mmState = s, mmPos = (Point x y), mmRel = (Point xr yr) }) = do
    pokeByteOff p  0 (4 :: Word8)
    pokeByteOff p  2 ((fromFlags s) :: Word8)
    pokeByteOff p  4 ((toEnum x)    :: Word16)
    pokeByteOff p  6 ((toEnum y)    :: Word16)
    pokeByteOff p  8 ((toEnum xr)   :: Int16)
    pokeByteOff p 10 ((toEnum yr)   :: Int16)

  poke p (MouseButtonEvent { mbPress = t, mbButton = b, mbState = s, mbPos = (Point x y) }) = do
    pokeByteOff p 0 ((if t then 5 else 6)      :: Word8)
    pokeByteOff p 2 ((toEnum ((fromEnum b)+1)) :: Word8)
    pokeByteOff p 3 ((fromBool s)              :: Word8)
    pokeByteOff p 4 ((toEnum x) :: Word16)
    pokeByteOff p 6 ((toEnum y) :: Word16)

  poke p (JoyAxisEvent { jaWhich = w, jaAxis = a, jaValue = v }) = do
    pokeByteOff p 0 (7 :: Word8)
    pokeByteOff p 1 ((toEnum w) :: Word8)
    pokeByteOff p 2 ((toEnum a) :: Word8)
    pokeByteOff p 4 ((toEnum v) :: Word16)

  poke p (JoyBallEvent { jbWhich = w, jbBall = b, jbRel = (Point x y) }) = do
    pokeByteOff p 0 (8 :: Word8)
    pokeByteOff p 1 ((toEnum w) :: Word8)
    pokeByteOff p 2 ((toEnum b) :: Word8)
    pokeByteOff p 4 ((toEnum x) :: Int16)
    pokeByteOff p 6 ((toEnum y) :: Int16)

  poke p (JoyHatEvent { jhWhich = w, jhHat = h, jhValue = v }) = do
    pokeByteOff p 0 (9 :: Word8)
    pokeByteOff p 1 ((toEnum w) :: Word8)
    pokeByteOff p 2 ((toEnum h) :: Word8)
    pokeByteOff p 3 ((toEnum v) :: Word8)

  poke p (JoyButtonEvent { jbType = t, jbWhich = w, jbButton = b, jbState = s }) = do
    pokeByteOff p 0 ((if t then 10 else 11) :: Word8)
    pokeByteOff p 1 ((toEnum w) :: Word8)
    pokeByteOff p 2 ((toEnum b) :: Word8)
    pokeByteOff p 3 ((fromBool s) :: Word8)

  poke p QuitEvent =
    pokeByteOff p 0 (12 :: Word8)

  poke p (ResizeEvent { rsSize = (Size w h) }) = do
    pokeByteOff p 0 (16 :: Word8)
    pokeByteOff p 4 w
    pokeByteOff p 8 h

  poke p ExposeEvent =
    pokeByteOff p 0 (17 :: Word8)

  poke p (UnknownEvent { ukType = t }) =
    pokeByteOff p 0 t

data EventFlag =
    NOEVENT | ACTIVEEVENT | KEYDOWN | KEYUP
  | MOUSEMOTION | MOUSEBUTTONDOWN | MOUSEBUTTONUP
  | JOYAXISMOTION | JOYBALLMOTION | JOYHATMOTION | JOYBUTTONDOWN | JOYBUTTONUP
  | QUIT | SYSWMEVENT | VIDEORESIZE | VIDEOEXPOSE | ALLEVENTS
  deriving (Eq,Show,Enum)

instance Flag EventFlag where
  fromFlag NOEVENT         = bit 0
  fromFlag ACTIVEEVENT     = bit 1
  fromFlag KEYDOWN         = bit 2
  fromFlag KEYUP           = bit 3
  fromFlag MOUSEMOTION     = bit 4
  fromFlag MOUSEBUTTONDOWN = bit 5
  fromFlag MOUSEBUTTONUP   = bit 6
  fromFlag JOYAXISMOTION   = bit 7
  fromFlag JOYBALLMOTION   = bit 8
  fromFlag JOYHATMOTION    = bit 9
  fromFlag JOYBUTTONDOWN   = bit 10
  fromFlag JOYBUTTONUP     = bit 11
  fromFlag QUIT            = bit 12
  fromFlag SYSWMEVENT      = bit 13
  fromFlag VIDEORESIZE     = bit 16
  fromFlag VIDEOEXPOSE     = bit 17
  fromFlag ALLEVENTS       = 0xFFFFFFFF

data AppStates =
  APPMOUSEFOCUS | APPINPUTFOCUS | APPACTIVE
  deriving (Eq,Show,Enum)

instance Flag AppStates where
  fromFlag APPMOUSEFOCUS = 0x01
  fromFlag APPINPUTFOCUS = 0x02
  fromFlag APPACTIVE     = 0x04

data MouseButton =
  BUTTON_LEFT | BUTTON_MIDDLE | BUTTON_RIGHT | BUTTON_WHEELUP | BUTTON_WHEELDOWN
  deriving (Eq,Show,Enum)

instance Flag MouseButton where
  fromFlag BUTTON_LEFT      = 0x01
  fromFlag BUTTON_MIDDLE    = 0x02
  fromFlag BUTTON_RIGHT     = 0x04
  fromFlag BUTTON_WHEELUP   = 0x08
  fromFlag BUTTON_WHEELDOWN = 0x10

data Keysym = Keysym
  { ksScancode :: Word8
  , ksSym      :: HSDLKey
  , ksMod      :: [HSDLMod]
  , ksUnicode  :: Word16
  }
  deriving (Eq,Show)

----------------

pumpEvents :: IO ()
pumpEvents = inSDLPumpEvents

-- sdlPeepEvent
-- そのままではどうにも汚いので、以下の2つの関数に分解
pushEvents :: [Event] -> IO Bool
pushEvents e =
  withArray e $ \p -> do
     ret <- inSDLPeepEvents p (length e) sdlAddevent 0
     return $ ret /= -1

pushEvent :: Event -> IO Bool
pushEvent e = pushEvents [e]

peekEvents :: Int -> [EventFlag] -> Bool -> IO (Maybe [Event])
peekEvents num ef remove =
  allocaArray num $ \p -> do
    ret <- inSDLPeepEvents p num action (fromFlags ef)
    if ret == -1
      then return Nothing
      else do
        ev <- peekArray ret p
        return $ Just ev
  where
  action = if remove then sdlGetevent else sdlPeekevent

type EventAction = #type SDL_eventaction
#enum EventAction, id, SDL_ADDEVENT, SDL_PEEKEVENT, SDL_GETEVENT

pollEvent :: IO (Maybe Event)
pollEvent = intPollEvent inSDLPollEvent

waitEvent :: IO (Maybe Event)
waitEvent = intPollEvent inSDLWaitEvent

intPollEvent c = 
  alloca $ \p -> do
    ret <- c p
    if ret==0 then return Nothing
      else do
        e <- peek p
        return $ Just e

setEventFilter :: (Event -> Bool) -> IO ()
setEventFilter f = do
  fp <- mkEventFilter (wrap f)
  inSDLSetEventFilter fp
  where
    wrap f ep = do
      ev <- peek ep
      return $ fromBool $ f ev

type EventFilter = Ptr Event -> IO Int
foreign import ccall "wrapper" mkEventFilter :: EventFilter -> IO (FunPtr EventFilter)

-- 未実装…IOがついてしまうので保留
-- sdlGetEventFilter :: IO (Event -> IO Bool)

getEventState :: EventFlag -> IO Bool
getEventState flag = do
  state <- inSDLEventState (fromFlag flag) sdlQuery
  return $ fromIntegral state == sdlEnable

setEventState :: EventFlag -> Bool -> IO ()
setEventState flag enable = do
  inSDLEventState (fromFlag flag) state
  return ()
  where
  state = if enable then sdlEnable else sdlIgnore

getKeyState :: IO [HSDLKey]
getKeyState =
  alloca $ \p -> do
    ptr <- inSDLGetKeyState p
    n   <- peek p
    ret <- peekArray n ptr
    return $ [toEnum a | a <- [0..n-1], (ret!!a)==1]

getModState :: IO [HSDLMod]
getModState = do
  ret <- inSDLGetModState
  return $ toFlags ret

setModState :: [HSDLMod] -> IO ()
setModState ms =
  inSDLSetModState $ fromFlags ms

getKeyName :: HSDLKey -> IO String
getKeyName k = do
  str <- inSDLGetKeyName $ fromEnum k
  peekCString str

enableUNICODE :: Maybe Bool -> IO Bool
enableUNICODE Nothing = do
  ret <- inSDLEnableUNICODE (-1)
  return $ toBool ret
enableUNICODE (Just e) = do
  ret <- inSDLEnableUNICODE $ fromBool e
  return $ toBool ret

enableKeyRepeat :: Int -> Int -> IO Bool
enableKeyRepeat delay interval = do
  ret <- inSDLEnableKeyRepeat delay interval
  return $ ret==0

getMouseState :: IO (Point,[MouseButton])
getMouseState = intMouseState inSDLGetMouseState

getRelativeMouseState :: IO (Point,[MouseButton])
getRelativeMouseState = intMouseState inSDLGetRelativeMouseState

intMouseState f =
  alloca $ \px ->
  alloca $ \py -> do
    ret <- f px py
    x <- peek px
    y <- peek py
    return $ (Point x y,toFlags ret)

getAppState :: IO [AppStates]
getAppState = do
  ret <- inSDLGetAppState
  return $ toFlags ret

----------------

#include <SDL.h>
#undef main

foreign import ccall "SDL.h SDL_PumpEvents" inSDLPumpEvents :: IO ()
foreign import ccall "SDL.h SDL_PeepEvents" inSDLPeepEvents :: Ptr Event -> Int -> EventAction -> Word32 -> IO Int
foreign import ccall "SDL.h SDL_PollEvent" inSDLPollEvent :: Ptr Event -> IO Int
foreign import ccall "SDL.h SDL_WaitEvent" inSDLWaitEvent :: Ptr Event -> IO Int
foreign import ccall "SDL.h SDL_PushEvent" inSDLPushEvent :: Ptr Event -> IO Int

foreign import ccall "SDL.h SDL_SetEventFilter" inSDLSetEventFilter :: FunPtr EventFilter -> IO ()
foreign import ccall "SDL.h SDL_GetEventFilter" inSDLGetEventFilter :: IO (FunPtr EventFilter)

foreign import ccall "SDL.h SDL_EventState"  inSDLEventState  :: Word8 -> EventState -> IO Word8
foreign import ccall "SDL.h SDL_GetKeyState" inSDLGetKeyState :: Ptr Int -> IO (Ptr Word8)
foreign import ccall "SDL.h SDL_GetModState" inSDLGetModState :: IO Word32
foreign import ccall "SDL.h SDL_SetModState" inSDLSetModState :: Word32 -> IO ()
foreign import ccall "SDL.h SDL_GetKeyName"  inSDLGetKeyName  :: Int -> IO CString

foreign import ccall "SDL.h SDL_EnableUNICODE"         inSDLEnableUNICODE         :: Int -> IO Int
foreign import ccall "SDL.h SDL_EnableKeyRepeat"       inSDLEnableKeyRepeat       :: Int -> Int -> IO Int
foreign import ccall "SDL.h SDL_GetMouseState"         inSDLGetMouseState         :: Ptr Int -> Ptr Int -> IO Word8
foreign import ccall "SDL.h SDL_GetRelativeMouseState" inSDLGetRelativeMouseState :: Ptr Int -> Ptr Int -> IO Word8
foreign import ccall "SDL.h SDL_GetAppState"           inSDLGetAppState           :: IO Word8

