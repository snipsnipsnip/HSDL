module Multimedia.HSDL.Keysym (
  SDLKey(..),
  SDLMod(..),
) where

import Multimedia.HSDL.Util

data SDLKey =
    SDLK_UNKNOWN   -- 0
  | SDLK_FIRST     -- 0
  | SDLK_BACKSPACE -- 8
  | SDLK_TAB       -- 9
  | SDLK_CLEAR     -- 12
  | SDLK_RETURN    -- 13
  | SDLK_PAUSE     -- 19
  | SDLK_ESCAPE    -- 27
  | SDLK_SPACE     -- 32
  | SDLK_EXCLAIM   -- 33
  | SDLK_QUOTEDBL  -- 34
  | SDLK_HASH      -- 35
  | SDLK_DOLLAR    -- 36
  | SDLK_AMPERSAND -- 38
  | SDLK_QUOTE     -- 39
  | SDLK_LEFTPAREN -- 40
  | SDLK_RIGHTPAREN-- 41
  | SDLK_ASTERISK  -- 42
  | SDLK_PLUS      -- 43
  | SDLK_COMMA     -- 44
  | SDLK_MINUS     -- 45
  | SDLK_PERIOD    -- 46
  | SDLK_SLASH     -- 47
  | SDLK_0         -- 48
  | SDLK_1         -- 49
  | SDLK_2         -- 50
  | SDLK_3         -- 51
  | SDLK_4         -- 52
  | SDLK_5         -- 53
  | SDLK_6         -- 54
  | SDLK_7         -- 55
  | SDLK_8         -- 56
  | SDLK_9         -- 57
  | SDLK_COLON     -- 58
  | SDLK_SEMICOLON -- 59
  | SDLK_LESS      -- 60
  | SDLK_EQUALS    -- 61
  | SDLK_GREATER   -- 62
  | SDLK_QUESTION  -- 63
  | SDLK_AT        -- 64
 {- 
   Skip uppercase letters
  -}
  | SDLK_LEFTBRACKET  -- 91
  | SDLK_BACKSLASH    -- 92
  | SDLK_RIGHTBRACKET -- 93
  | SDLK_CARE         -- 94
  | SDLK_UNDERSCORE   -- 95
  | SDLK_BACKQUOTE    -- 96

  | SDLK_a -- 97
  | SDLK_b -- 98
  | SDLK_c -- 99
  | SDLK_d -- 100
  | SDLK_e -- 101
  | SDLK_f -- 102
  | SDLK_g -- 103
  | SDLK_h -- 104
  | SDLK_i -- 105
  | SDLK_j -- 106
  | SDLK_k -- 107
  | SDLK_l -- 108
  | SDLK_m -- 109
  | SDLK_n -- 110
  | SDLK_o -- 111
  | SDLK_p -- 112
  | SDLK_q -- 113
  | SDLK_r -- 114
  | SDLK_s -- 115
  | SDLK_t -- 116
  | SDLK_u -- 117
  | SDLK_v -- 118
  | SDLK_w -- 119
  | SDLK_x -- 120
  | SDLK_y -- 121
  | SDLK_z -- 122
  | SDLK_DELETE -- 127
  -- End of ASCII mapped keysyms

  -- International keyboard syms
  | SDLK_WORLD_0  -- 160 {- 0xA0 -}
  | SDLK_WORLD_1  -- 161
  | SDLK_WORLD_2  -- 162
  | SDLK_WORLD_3  -- 163
  | SDLK_WORLD_4  -- 164
  | SDLK_WORLD_5  -- 165
  | SDLK_WORLD_6  -- 166
  | SDLK_WORLD_7  -- 167
  | SDLK_WORLD_8  -- 168
  | SDLK_WORLD_9  -- 169
  | SDLK_WORLD_10 -- 170
  | SDLK_WORLD_11 -- 171
  | SDLK_WORLD_12 -- 172
  | SDLK_WORLD_13 -- 173
  | SDLK_WORLD_14 -- 174
  | SDLK_WORLD_15 -- 175
  | SDLK_WORLD_16 -- 176
  | SDLK_WORLD_17 -- 177
  | SDLK_WORLD_18 -- 178
  | SDLK_WORLD_19 -- 179
  | SDLK_WORLD_20 -- 180
  | SDLK_WORLD_21 -- 181
  | SDLK_WORLD_22 -- 182
  | SDLK_WORLD_23 -- 183
  | SDLK_WORLD_24 -- 184
  | SDLK_WORLD_25 -- 185
  | SDLK_WORLD_26 -- 186
  | SDLK_WORLD_27 -- 187
  | SDLK_WORLD_28 -- 188
  | SDLK_WORLD_29 -- 189
  | SDLK_WORLD_30 -- 190
  | SDLK_WORLD_31 -- 191
  | SDLK_WORLD_32 -- 192
  | SDLK_WORLD_33 -- 193
  | SDLK_WORLD_34 -- 194
  | SDLK_WORLD_35 -- 195
  | SDLK_WORLD_36 -- 196
  | SDLK_WORLD_37 -- 197
  | SDLK_WORLD_38 -- 198
  | SDLK_WORLD_39 -- 199
  | SDLK_WORLD_40 -- 200
  | SDLK_WORLD_41 -- 201
  | SDLK_WORLD_42 -- 202
  | SDLK_WORLD_43 -- 203
  | SDLK_WORLD_44 -- 204
  | SDLK_WORLD_45 -- 205
  | SDLK_WORLD_46 -- 206
  | SDLK_WORLD_47 -- 207
  | SDLK_WORLD_48 -- 208
  | SDLK_WORLD_49 -- 209
  | SDLK_WORLD_50 -- 210
  | SDLK_WORLD_51 -- 211
  | SDLK_WORLD_52 -- 212
  | SDLK_WORLD_53 -- 213
  | SDLK_WORLD_54 -- 214
  | SDLK_WORLD_55 -- 215
  | SDLK_WORLD_56 -- 216
  | SDLK_WORLD_57 -- 217
  | SDLK_WORLD_58 -- 218
  | SDLK_WORLD_59 -- 219
  | SDLK_WORLD_60 -- 220
  | SDLK_WORLD_61 -- 221
  | SDLK_WORLD_62 -- 222
  | SDLK_WORLD_63 -- 223
  | SDLK_WORLD_64 -- 224
  | SDLK_WORLD_65 -- 225
  | SDLK_WORLD_66 -- 226
  | SDLK_WORLD_67 -- 227
  | SDLK_WORLD_68 -- 228
  | SDLK_WORLD_69 -- 229
  | SDLK_WORLD_70 -- 230
  | SDLK_WORLD_71 -- 231
  | SDLK_WORLD_72 -- 232
  | SDLK_WORLD_73 -- 233
  | SDLK_WORLD_74 -- 234
  | SDLK_WORLD_75 -- 235
  | SDLK_WORLD_76 -- 236
  | SDLK_WORLD_77 -- 237
  | SDLK_WORLD_78 -- 238
  | SDLK_WORLD_79 -- 239
  | SDLK_WORLD_80 -- 240
  | SDLK_WORLD_81 -- 241
  | SDLK_WORLD_82 -- 242
  | SDLK_WORLD_83 -- 243
  | SDLK_WORLD_84 -- 244
  | SDLK_WORLD_85 -- 245
  | SDLK_WORLD_86 -- 246
  | SDLK_WORLD_87 -- 247
  | SDLK_WORLD_88 -- 248
  | SDLK_WORLD_89 -- 249
  | SDLK_WORLD_90 -- 250
  | SDLK_WORLD_91 -- 251
  | SDLK_WORLD_92 -- 252
  | SDLK_WORLD_93 -- 253
  | SDLK_WORLD_94 -- 254
  | SDLK_WORLD_95 -- 255 {- 0xFF -}

  -- Numeric keypad
  | SDLK_KP0 -- 256
  | SDLK_KP1 -- 257
  | SDLK_KP2 -- 258
  | SDLK_KP3 -- 259
  | SDLK_KP4 -- 260
  | SDLK_KP5 -- 261
  | SDLK_KP6 -- 262
  | SDLK_KP7 -- 263
  | SDLK_KP8 -- 264
  | SDLK_KP9 -- 265
  | SDLK_KP_PERIOD   -- 266
  | SDLK_KP_DIVIDE   -- 267
  | SDLK_KP_MULTIPLY -- 268
  | SDLK_KP_MINUS    -- 269
  | SDLK_KP_PLUS     -- 270
  | SDLK_KP_ENTER    -- 271
  | SDLK_KP_EQUALS   -- 272

  -- Arrows + Home/End pad
  | SDLK_UP       -- 273
  | SDLK_DOWN     -- 274
  | SDLK_RIGHT    -- 275
  | SDLK_LEFT     -- 276
  | SDLK_INSERT   -- 277
  | SDLK_HOME     -- 278
  | SDLK_END      -- 279
  | SDLK_PAGEUP   -- 280
  | SDLK_PAGEDOWN -- 281

  -- Function keys
  | SDLK_F1  -- 282
  | SDLK_F2  -- 283
  | SDLK_F3  -- 284
  | SDLK_F4  -- 285
  | SDLK_F5  -- 286
  | SDLK_F6  -- 287
  | SDLK_F7  -- 288
  | SDLK_F8  -- 289
  | SDLK_F9  -- 290
  | SDLK_F10 -- 291
  | SDLK_F11 -- 292
  | SDLK_F12 -- 293
  | SDLK_F13 -- 294
  | SDLK_F14 -- 295
  | SDLK_F15 -- 296

  -- Key state modifier keys
  | SDLK_NUMLOCK   -- 300
  | SDLK_CAPSLOCK  -- 301
  | SDLK_SCROLLOCK -- 302
  | SDLK_RSHIFT    -- 303
  | SDLK_LSHIFT    -- 304
  | SDLK_RCTRL     -- 305
  | SDLK_LCTRL     -- 306
  | SDLK_RALT      -- 307
  | SDLK_LALT      -- 308
  | SDLK_RMETA     -- 309
  | SDLK_LMETA     -- 310
  | SDLK_LSUPER    -- 311 -- Left "Windows" key
  | SDLK_RSUPER    -- 312 -- Right "Windows" key
  | SDLK_MODE      -- 313 -- "Alt Gr" key
  | SDLK_COMPOSE   -- 314 -- Multi-key compose key

  -- Miscellaneous function keys
  | SDLK_HELP   -- 315
  | SDLK_PRINT  -- 316
  | SDLK_SYSREQ -- 317
  | SDLK_BREAK  -- 318
  | SDLK_MENU   -- 319
  | SDLK_POWER  -- 320 -- Power Macintosh power key
  | SDLK_EURO   -- 321 -- Some european keyboards
  | SDLK_UNDO   -- 322 -- Atari keyboard has Undo

  -- Add any other keys here
  | SDLK_LAST
  deriving (Eq,Show)

instance Enum SDLKey where
  fromEnum SDLK_UNKNOWN   = 0
  fromEnum SDLK_FIRST     = 0
  fromEnum SDLK_BACKSPACE = 8
  fromEnum SDLK_TAB       = 9
  fromEnum SDLK_CLEAR     = 12
  fromEnum SDLK_RETURN    = 13
  fromEnum SDLK_PAUSE     = 19
  fromEnum SDLK_ESCAPE    = 27
  fromEnum SDLK_SPACE     = 32
  fromEnum SDLK_EXCLAIM   = 33
  fromEnum SDLK_QUOTEDBL  = 34
  fromEnum SDLK_HASH      = 35
  fromEnum SDLK_DOLLAR    = 36
  fromEnum SDLK_AMPERSAND = 38
  fromEnum SDLK_QUOTE     = 39
  fromEnum SDLK_LEFTPAREN = 40
  fromEnum SDLK_RIGHTPAREN= 41
  fromEnum SDLK_ASTERISK  = 42
  fromEnum SDLK_PLUS      = 43
  fromEnum SDLK_COMMA     = 44
  fromEnum SDLK_MINUS     = 45
  fromEnum SDLK_PERIOD    = 46
  fromEnum SDLK_SLASH     = 47
  fromEnum SDLK_0         = 48
  fromEnum SDLK_1         = 49
  fromEnum SDLK_2         = 50
  fromEnum SDLK_3         = 51
  fromEnum SDLK_4         = 52
  fromEnum SDLK_5         = 53
  fromEnum SDLK_6         = 54
  fromEnum SDLK_7         = 55
  fromEnum SDLK_8         = 56
  fromEnum SDLK_9         = 57
  fromEnum SDLK_COLON     = 58
  fromEnum SDLK_SEMICOLON = 59
  fromEnum SDLK_LESS      = 60
  fromEnum SDLK_EQUALS    = 61
  fromEnum SDLK_GREATER   = 62
  fromEnum SDLK_QUESTION  = 63
  fromEnum SDLK_AT        = 64

  fromEnum SDLK_LEFTBRACKET  = 91
  fromEnum SDLK_BACKSLASH    = 92
  fromEnum SDLK_RIGHTBRACKET = 93
  fromEnum SDLK_CARE         = 94
  fromEnum SDLK_UNDERSCORE   = 95
  fromEnum SDLK_BACKQUOTE    = 96

  fromEnum SDLK_a = 97
  fromEnum SDLK_b = 98
  fromEnum SDLK_c = 99
  fromEnum SDLK_d = 100
  fromEnum SDLK_e = 101
  fromEnum SDLK_f = 102
  fromEnum SDLK_g = 103
  fromEnum SDLK_h = 104
  fromEnum SDLK_i = 105
  fromEnum SDLK_j = 106
  fromEnum SDLK_k = 107
  fromEnum SDLK_l = 108
  fromEnum SDLK_m = 109
  fromEnum SDLK_n = 110
  fromEnum SDLK_o = 111
  fromEnum SDLK_p = 112
  fromEnum SDLK_q = 113
  fromEnum SDLK_r = 114
  fromEnum SDLK_s = 115
  fromEnum SDLK_t = 116
  fromEnum SDLK_u = 117
  fromEnum SDLK_v = 118
  fromEnum SDLK_w = 119
  fromEnum SDLK_x = 120
  fromEnum SDLK_y = 121
  fromEnum SDLK_z = 122
  fromEnum SDLK_DELETE = 127

  fromEnum SDLK_WORLD_0  = 160
  fromEnum SDLK_WORLD_1  = 161
  fromEnum SDLK_WORLD_2  = 162
  fromEnum SDLK_WORLD_3  = 163
  fromEnum SDLK_WORLD_4  = 164
  fromEnum SDLK_WORLD_5  = 165
  fromEnum SDLK_WORLD_6  = 166
  fromEnum SDLK_WORLD_7  = 167
  fromEnum SDLK_WORLD_8  = 168
  fromEnum SDLK_WORLD_9  = 169
  fromEnum SDLK_WORLD_10 = 170
  fromEnum SDLK_WORLD_11 = 171
  fromEnum SDLK_WORLD_12 = 172
  fromEnum SDLK_WORLD_13 = 173
  fromEnum SDLK_WORLD_14 = 174
  fromEnum SDLK_WORLD_15 = 175
  fromEnum SDLK_WORLD_16 = 176
  fromEnum SDLK_WORLD_17 = 177
  fromEnum SDLK_WORLD_18 = 178
  fromEnum SDLK_WORLD_19 = 179
  fromEnum SDLK_WORLD_20 = 180
  fromEnum SDLK_WORLD_21 = 181
  fromEnum SDLK_WORLD_22 = 182
  fromEnum SDLK_WORLD_23 = 183
  fromEnum SDLK_WORLD_24 = 184
  fromEnum SDLK_WORLD_25 = 185
  fromEnum SDLK_WORLD_26 = 186
  fromEnum SDLK_WORLD_27 = 187
  fromEnum SDLK_WORLD_28 = 188
  fromEnum SDLK_WORLD_29 = 189
  fromEnum SDLK_WORLD_30 = 190
  fromEnum SDLK_WORLD_31 = 191
  fromEnum SDLK_WORLD_32 = 192
  fromEnum SDLK_WORLD_33 = 193
  fromEnum SDLK_WORLD_34 = 194
  fromEnum SDLK_WORLD_35 = 195
  fromEnum SDLK_WORLD_36 = 196
  fromEnum SDLK_WORLD_37 = 197
  fromEnum SDLK_WORLD_38 = 198
  fromEnum SDLK_WORLD_39 = 199
  fromEnum SDLK_WORLD_40 = 200
  fromEnum SDLK_WORLD_41 = 201
  fromEnum SDLK_WORLD_42 = 202
  fromEnum SDLK_WORLD_43 = 203
  fromEnum SDLK_WORLD_44 = 204
  fromEnum SDLK_WORLD_45 = 205
  fromEnum SDLK_WORLD_46 = 206
  fromEnum SDLK_WORLD_47 = 207
  fromEnum SDLK_WORLD_48 = 208
  fromEnum SDLK_WORLD_49 = 209
  fromEnum SDLK_WORLD_50 = 210
  fromEnum SDLK_WORLD_51 = 211
  fromEnum SDLK_WORLD_52 = 212
  fromEnum SDLK_WORLD_53 = 213
  fromEnum SDLK_WORLD_54 = 214
  fromEnum SDLK_WORLD_55 = 215
  fromEnum SDLK_WORLD_56 = 216
  fromEnum SDLK_WORLD_57 = 217
  fromEnum SDLK_WORLD_58 = 218
  fromEnum SDLK_WORLD_59 = 219
  fromEnum SDLK_WORLD_60 = 220
  fromEnum SDLK_WORLD_61 = 221
  fromEnum SDLK_WORLD_62 = 222
  fromEnum SDLK_WORLD_63 = 223
  fromEnum SDLK_WORLD_64 = 224
  fromEnum SDLK_WORLD_65 = 225
  fromEnum SDLK_WORLD_66 = 226
  fromEnum SDLK_WORLD_67 = 227
  fromEnum SDLK_WORLD_68 = 228
  fromEnum SDLK_WORLD_69 = 229
  fromEnum SDLK_WORLD_70 = 230
  fromEnum SDLK_WORLD_71 = 231
  fromEnum SDLK_WORLD_72 = 232
  fromEnum SDLK_WORLD_73 = 233
  fromEnum SDLK_WORLD_74 = 234
  fromEnum SDLK_WORLD_75 = 235
  fromEnum SDLK_WORLD_76 = 236
  fromEnum SDLK_WORLD_77 = 237
  fromEnum SDLK_WORLD_78 = 238
  fromEnum SDLK_WORLD_79 = 239
  fromEnum SDLK_WORLD_80 = 240
  fromEnum SDLK_WORLD_81 = 241
  fromEnum SDLK_WORLD_82 = 242
  fromEnum SDLK_WORLD_83 = 243
  fromEnum SDLK_WORLD_84 = 244
  fromEnum SDLK_WORLD_85 = 245
  fromEnum SDLK_WORLD_86 = 246
  fromEnum SDLK_WORLD_87 = 247
  fromEnum SDLK_WORLD_88 = 248
  fromEnum SDLK_WORLD_89 = 249
  fromEnum SDLK_WORLD_90 = 250
  fromEnum SDLK_WORLD_91 = 251
  fromEnum SDLK_WORLD_92 = 252
  fromEnum SDLK_WORLD_93 = 253
  fromEnum SDLK_WORLD_94 = 254
  fromEnum SDLK_WORLD_95 = 255

  fromEnum SDLK_KP0 = 256
  fromEnum SDLK_KP1 = 257
  fromEnum SDLK_KP2 = 258
  fromEnum SDLK_KP3 = 259
  fromEnum SDLK_KP4 = 260
  fromEnum SDLK_KP5 = 261
  fromEnum SDLK_KP6 = 262
  fromEnum SDLK_KP7 = 263
  fromEnum SDLK_KP8 = 264
  fromEnum SDLK_KP9 = 265
  fromEnum SDLK_KP_PERIOD   = 266
  fromEnum SDLK_KP_DIVIDE   = 267
  fromEnum SDLK_KP_MULTIPLY = 268
  fromEnum SDLK_KP_MINUS    = 269
  fromEnum SDLK_KP_PLUS     = 270
  fromEnum SDLK_KP_ENTER    = 271
  fromEnum SDLK_KP_EQUALS   = 272

  fromEnum SDLK_UP       = 273
  fromEnum SDLK_DOWN     = 274
  fromEnum SDLK_RIGHT    = 275
  fromEnum SDLK_LEFT     = 276
  fromEnum SDLK_INSERT   = 277
  fromEnum SDLK_HOME     = 278
  fromEnum SDLK_END      = 279
  fromEnum SDLK_PAGEUP   = 280
  fromEnum SDLK_PAGEDOWN = 281

  fromEnum SDLK_F1  = 282
  fromEnum SDLK_F2  = 283
  fromEnum SDLK_F3  = 284
  fromEnum SDLK_F4  = 285
  fromEnum SDLK_F5  = 286
  fromEnum SDLK_F6  = 287
  fromEnum SDLK_F7  = 288
  fromEnum SDLK_F8  = 289
  fromEnum SDLK_F9  = 290
  fromEnum SDLK_F10 = 291
  fromEnum SDLK_F11 = 292
  fromEnum SDLK_F12 = 293
  fromEnum SDLK_F13 = 294
  fromEnum SDLK_F14 = 295
  fromEnum SDLK_F15 = 296

  fromEnum SDLK_NUMLOCK   = 300
  fromEnum SDLK_CAPSLOCK  = 301
  fromEnum SDLK_SCROLLOCK = 302
  fromEnum SDLK_RSHIFT    = 303
  fromEnum SDLK_LSHIFT    = 304
  fromEnum SDLK_RCTRL     = 305
  fromEnum SDLK_LCTRL     = 306
  fromEnum SDLK_RALT      = 307
  fromEnum SDLK_LALT      = 308
  fromEnum SDLK_RMETA     = 309
  fromEnum SDLK_LMETA     = 310
  fromEnum SDLK_LSUPER    = 311
  fromEnum SDLK_RSUPER    = 312
  fromEnum SDLK_MODE      = 313
  fromEnum SDLK_COMPOSE   = 314

  fromEnum SDLK_HELP   = 315
  fromEnum SDLK_PRINT  = 316
  fromEnum SDLK_SYSREQ = 317
  fromEnum SDLK_BREAK  = 318
  fromEnum SDLK_MENU   = 319
  fromEnum SDLK_POWER  = 320
  fromEnum SDLK_EURO   = 321
  fromEnum SDLK_UNDO   = 322

  fromEnum SDLK_LAST   = 323


  toEnum  0 = SDLK_FIRST     -- 0
  toEnum  8 = SDLK_BACKSPACE -- 8
  toEnum  9 = SDLK_TAB       -- 9
  toEnum 12 = SDLK_CLEAR     -- 12
  toEnum 13 = SDLK_RETURN    -- 13
  toEnum 19 = SDLK_PAUSE     -- 19
  toEnum 27 = SDLK_ESCAPE    -- 27
  toEnum 32 = SDLK_SPACE     -- 32
  toEnum 33 = SDLK_EXCLAIM   -- 33
  toEnum 34 = SDLK_QUOTEDBL  -- 34
  toEnum 35 = SDLK_HASH      -- 35
  toEnum 36 = SDLK_DOLLAR    -- 36
  toEnum 38 = SDLK_AMPERSAND -- 38
  toEnum 39 = SDLK_QUOTE     -- 39
  toEnum 40 = SDLK_LEFTPAREN -- 40
  toEnum 41 = SDLK_RIGHTPAREN-- 41
  toEnum 42 = SDLK_ASTERISK  -- 42
  toEnum 43 = SDLK_PLUS      -- 43
  toEnum 44 = SDLK_COMMA     -- 44
  toEnum 45 = SDLK_MINUS     -- 45
  toEnum 46 = SDLK_PERIOD    -- 46
  toEnum 47 = SDLK_SLASH     -- 47
  toEnum 48 = SDLK_0         -- 48
  toEnum 49 = SDLK_1         -- 49
  toEnum 50 = SDLK_2         -- 50
  toEnum 51 = SDLK_3         -- 51
  toEnum 52 = SDLK_4         -- 52
  toEnum 53 = SDLK_5         -- 53
  toEnum 54 = SDLK_6         -- 54
  toEnum 55 = SDLK_7         -- 55
  toEnum 56 = SDLK_8         -- 56
  toEnum 57 = SDLK_9         -- 57
  toEnum 58 = SDLK_COLON     -- 58
  toEnum 59 = SDLK_SEMICOLON -- 59
  toEnum 60 = SDLK_LESS      -- 60
  toEnum 61 = SDLK_EQUALS    -- 61
  toEnum 62 = SDLK_GREATER   -- 62
  toEnum 63 = SDLK_QUESTION  -- 63
  toEnum 64 = SDLK_AT        -- 64

  toEnum 91 = SDLK_LEFTBRACKET  -- 91
  toEnum 92 = SDLK_BACKSLASH    -- 92
  toEnum 93 = SDLK_RIGHTBRACKET -- 93
  toEnum 94 = SDLK_CARE         -- 94
  toEnum 95 = SDLK_UNDERSCORE   -- 95
  toEnum 96 = SDLK_BACKQUOTE    -- 96

  toEnum  97 = SDLK_a -- 97
  toEnum  98 = SDLK_b -- 98
  toEnum  99 = SDLK_c -- 99
  toEnum 100 = SDLK_d -- 100
  toEnum 101 = SDLK_e -- 101
  toEnum 102 = SDLK_f -- 102
  toEnum 103 = SDLK_g -- 103
  toEnum 104 = SDLK_h -- 104
  toEnum 105 = SDLK_i -- 105
  toEnum 106 = SDLK_j -- 106
  toEnum 107 = SDLK_k -- 107
  toEnum 108 = SDLK_l -- 108
  toEnum 109 = SDLK_m -- 109
  toEnum 110 = SDLK_n -- 110
  toEnum 111 = SDLK_o -- 111
  toEnum 112 = SDLK_p -- 112
  toEnum 113 = SDLK_q -- 113
  toEnum 114 = SDLK_r -- 114
  toEnum 115 = SDLK_s -- 115
  toEnum 116 = SDLK_t -- 116
  toEnum 117 = SDLK_u -- 117
  toEnum 118 = SDLK_v -- 118
  toEnum 119 = SDLK_w -- 119
  toEnum 120 = SDLK_x -- 120
  toEnum 121 = SDLK_y -- 121
  toEnum 122 = SDLK_z -- 122
  toEnum 127 = SDLK_DELETE -- 127

  toEnum 160 = SDLK_WORLD_0  -- 160
  toEnum 161 = SDLK_WORLD_1  -- 161
  toEnum 162 = SDLK_WORLD_2  -- 162
  toEnum 163 = SDLK_WORLD_3  -- 163
  toEnum 164 = SDLK_WORLD_4  -- 164
  toEnum 165 = SDLK_WORLD_5  -- 165
  toEnum 166 = SDLK_WORLD_6  -- 166
  toEnum 167 = SDLK_WORLD_7  -- 167
  toEnum 168 = SDLK_WORLD_8  -- 168
  toEnum 169 = SDLK_WORLD_9  -- 169
  toEnum 170 = SDLK_WORLD_10 -- 170
  toEnum 171 = SDLK_WORLD_11 -- 171
  toEnum 172 = SDLK_WORLD_12 -- 172
  toEnum 173 = SDLK_WORLD_13 -- 173
  toEnum 174 = SDLK_WORLD_14 -- 174
  toEnum 175 = SDLK_WORLD_15 -- 175
  toEnum 176 = SDLK_WORLD_16 -- 176
  toEnum 177 = SDLK_WORLD_17 -- 177
  toEnum 178 = SDLK_WORLD_18 -- 178
  toEnum 179 = SDLK_WORLD_19 -- 179
  toEnum 180 = SDLK_WORLD_20 -- 180
  toEnum 181 = SDLK_WORLD_21 -- 181
  toEnum 182 = SDLK_WORLD_22 -- 182
  toEnum 183 = SDLK_WORLD_23 -- 183
  toEnum 184 = SDLK_WORLD_24 -- 184
  toEnum 185 = SDLK_WORLD_25 -- 185
  toEnum 186 = SDLK_WORLD_26 -- 186
  toEnum 187 = SDLK_WORLD_27 -- 187
  toEnum 188 = SDLK_WORLD_28 -- 188
  toEnum 189 = SDLK_WORLD_29 -- 189
  toEnum 190 = SDLK_WORLD_30 -- 190
  toEnum 191 = SDLK_WORLD_31 -- 191
  toEnum 192 = SDLK_WORLD_32 -- 192
  toEnum 193 = SDLK_WORLD_33 -- 193
  toEnum 194 = SDLK_WORLD_34 -- 194
  toEnum 195 = SDLK_WORLD_35 -- 195
  toEnum 196 = SDLK_WORLD_36 -- 196
  toEnum 197 = SDLK_WORLD_37 -- 197
  toEnum 198 = SDLK_WORLD_38 -- 198
  toEnum 199 = SDLK_WORLD_39 -- 199
  toEnum 200 = SDLK_WORLD_40 -- 200
  toEnum 201 = SDLK_WORLD_41 -- 201
  toEnum 202 = SDLK_WORLD_42 -- 202
  toEnum 203 = SDLK_WORLD_43 -- 203
  toEnum 204 = SDLK_WORLD_44 -- 204
  toEnum 205 = SDLK_WORLD_45 -- 205
  toEnum 206 = SDLK_WORLD_46 -- 206
  toEnum 207 = SDLK_WORLD_47 -- 207
  toEnum 208 = SDLK_WORLD_48 -- 208
  toEnum 209 = SDLK_WORLD_49 -- 209
  toEnum 210 = SDLK_WORLD_50 -- 210
  toEnum 211 = SDLK_WORLD_51 -- 211
  toEnum 212 = SDLK_WORLD_52 -- 212
  toEnum 213 = SDLK_WORLD_53 -- 213
  toEnum 214 = SDLK_WORLD_54 -- 214
  toEnum 215 = SDLK_WORLD_55 -- 215
  toEnum 216 = SDLK_WORLD_56 -- 216
  toEnum 217 = SDLK_WORLD_57 -- 217
  toEnum 218 = SDLK_WORLD_58 -- 218
  toEnum 219 = SDLK_WORLD_59 -- 219
  toEnum 220 = SDLK_WORLD_60 -- 220
  toEnum 221 = SDLK_WORLD_61 -- 221
  toEnum 222 = SDLK_WORLD_62 -- 222
  toEnum 223 = SDLK_WORLD_63 -- 223
  toEnum 224 = SDLK_WORLD_64 -- 224
  toEnum 225 = SDLK_WORLD_65 -- 225
  toEnum 226 = SDLK_WORLD_66 -- 226
  toEnum 227 = SDLK_WORLD_67 -- 227
  toEnum 228 = SDLK_WORLD_68 -- 228
  toEnum 229 = SDLK_WORLD_69 -- 229
  toEnum 230 = SDLK_WORLD_70 -- 230
  toEnum 231 = SDLK_WORLD_71 -- 231
  toEnum 232 = SDLK_WORLD_72 -- 232
  toEnum 233 = SDLK_WORLD_73 -- 233
  toEnum 234 = SDLK_WORLD_74 -- 234
  toEnum 235 = SDLK_WORLD_75 -- 235
  toEnum 236 = SDLK_WORLD_76 -- 236
  toEnum 237 = SDLK_WORLD_77 -- 237
  toEnum 238 = SDLK_WORLD_78 -- 238
  toEnum 239 = SDLK_WORLD_79 -- 239
  toEnum 240 = SDLK_WORLD_80 -- 240
  toEnum 241 = SDLK_WORLD_81 -- 241
  toEnum 242 = SDLK_WORLD_82 -- 242
  toEnum 243 = SDLK_WORLD_83 -- 243
  toEnum 244 = SDLK_WORLD_84 -- 244
  toEnum 245 = SDLK_WORLD_85 -- 245
  toEnum 246 = SDLK_WORLD_86 -- 246
  toEnum 247 = SDLK_WORLD_87 -- 247
  toEnum 248 = SDLK_WORLD_88 -- 248
  toEnum 249 = SDLK_WORLD_89 -- 249
  toEnum 250 = SDLK_WORLD_90 -- 250
  toEnum 251 = SDLK_WORLD_91 -- 251
  toEnum 252 = SDLK_WORLD_92 -- 252
  toEnum 253 = SDLK_WORLD_93 -- 253
  toEnum 254 = SDLK_WORLD_94 -- 254
  toEnum 255 = SDLK_WORLD_95 -- 255

  toEnum 256 = SDLK_KP0 -- 256
  toEnum 257 = SDLK_KP1 -- 257
  toEnum 258 = SDLK_KP2 -- 258
  toEnum 259 = SDLK_KP3 -- 259
  toEnum 260 = SDLK_KP4 -- 260
  toEnum 261 = SDLK_KP5 -- 261
  toEnum 262 = SDLK_KP6 -- 262
  toEnum 263 = SDLK_KP7 -- 263
  toEnum 264 = SDLK_KP8 -- 264
  toEnum 265 = SDLK_KP9 -- 265
  toEnum 266 = SDLK_KP_PERIOD   -- 266
  toEnum 267 = SDLK_KP_DIVIDE   -- 267
  toEnum 268 = SDLK_KP_MULTIPLY -- 268
  toEnum 269 = SDLK_KP_MINUS    -- 269
  toEnum 270 = SDLK_KP_PLUS     -- 270
  toEnum 271 = SDLK_KP_ENTER    -- 271
  toEnum 272 = SDLK_KP_EQUALS   -- 272

  toEnum 273 = SDLK_UP       -- 273
  toEnum 274 = SDLK_DOWN     -- 274
  toEnum 275 = SDLK_RIGHT    -- 275
  toEnum 276 = SDLK_LEFT     -- 276
  toEnum 277 = SDLK_INSERT   -- 277
  toEnum 278 = SDLK_HOME     -- 278
  toEnum 279 = SDLK_END      -- 279
  toEnum 280 = SDLK_PAGEUP   -- 280
  toEnum 281 = SDLK_PAGEDOWN -- 281

  toEnum 282 = SDLK_F1  -- 282
  toEnum 283 = SDLK_F2  -- 283
  toEnum 284 = SDLK_F3  -- 284
  toEnum 285 = SDLK_F4  -- 285
  toEnum 286 = SDLK_F5  -- 286
  toEnum 287 = SDLK_F6  -- 287
  toEnum 288 = SDLK_F7  -- 288
  toEnum 289 = SDLK_F8  -- 289
  toEnum 290 = SDLK_F9  -- 290
  toEnum 291 = SDLK_F10 -- 291
  toEnum 292 = SDLK_F11 -- 292
  toEnum 293 = SDLK_F12 -- 293
  toEnum 294 = SDLK_F13 -- 294
  toEnum 295 = SDLK_F14 -- 295
  toEnum 296 = SDLK_F15 -- 296

  toEnum 300 = SDLK_NUMLOCK   -- 300
  toEnum 301 = SDLK_CAPSLOCK  -- 301
  toEnum 302 = SDLK_SCROLLOCK -- 302
  toEnum 303 = SDLK_RSHIFT    -- 303
  toEnum 304 = SDLK_LSHIFT    -- 304
  toEnum 305 = SDLK_RCTRL     -- 305
  toEnum 306 = SDLK_LCTRL     -- 306
  toEnum 307 = SDLK_RALT      -- 307
  toEnum 308 = SDLK_LALT      -- 308
  toEnum 309 = SDLK_RMETA     -- 309
  toEnum 310 = SDLK_LMETA     -- 310
  toEnum 311 = SDLK_LSUPER    -- 311
  toEnum 312 = SDLK_RSUPER    -- 312
  toEnum 313 = SDLK_MODE      -- 313
  toEnum 314 = SDLK_COMPOSE   -- 314

  toEnum 315 = SDLK_HELP   -- 315
  toEnum 316 = SDLK_PRINT  -- 316
  toEnum 317 = SDLK_SYSREQ -- 317
  toEnum 318 = SDLK_BREAK  -- 318
  toEnum 319 = SDLK_MENU   -- 319
  toEnum 320 = SDLK_POWER  -- 320
  toEnum 321 = SDLK_EURO   -- 321
  toEnum 322 = SDLK_UNDO   -- 322

  toEnum 323 = SDLK_LAST   -- 323
  toEnum _   = SDLK_UNKNOWN

data SDLMod =
    KMOD_NONE
  | KMOD_LSHIFT
  | KMOD_RSHIFT
  | KMOD_LCTRL
  | KMOD_RCTRL
  | KMOD_LALT
  | KMOD_RALT
  | KMOD_LMETA
  | KMOD_RMETA
  | KMOD_NUM
  | KMOD_CAPS
  | KMOD_MODE
  | KMOD_RESERVED

  | KMOD_CTRL
  | KMOD_SHIFT
  | KMOD_ALT
  | KMOD_META
  deriving (Eq,Show,Enum)

instance Flag SDLMod where
  fromFlag KMOD_NONE  = 0x0000
  fromFlag KMOD_LSHIFT= 0x0001
  fromFlag KMOD_RSHIFT= 0x0002
  fromFlag KMOD_LCTRL = 0x0040
  fromFlag KMOD_RCTRL = 0x0080
  fromFlag KMOD_LALT  = 0x0100
  fromFlag KMOD_RALT  = 0x0200
  fromFlag KMOD_LMETA = 0x0400
  fromFlag KMOD_RMETA = 0x0800
  fromFlag KMOD_NUM   = 0x1000
  fromFlag KMOD_CAPS  = 0x2000
  fromFlag KMOD_MODE  = 0x4000
  fromFlag KMOD_RESERVED = 0x8000

  fromFlag KMOD_CTRL  = 0x00C0
  fromFlag KMOD_SHIFT = 0x0003
  fromFlag KMOD_ALT   = 0x0300
  fromFlag KMOD_META  = 0x0C00
