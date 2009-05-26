module Multimedia.HSDL.Keysym (
  SDLKey(..),
  SDLMod(..),
) where

import Multimedia.HSDL.Util

data SDLKey =
    HSDLK_UNKNOWN   -- 0
  | HSDLK_FIRST     -- 0
  | HSDLK_BACKSPACE -- 8
  | HSDLK_TAB       -- 9
  | HSDLK_CLEAR     -- 12
  | HSDLK_RETURN    -- 13
  | HSDLK_PAUSE     -- 19
  | HSDLK_ESCAPE    -- 27
  | HSDLK_SPACE     -- 32
  | HSDLK_EXCLAIM   -- 33
  | HSDLK_QUOTEDBL  -- 34
  | HSDLK_HASH      -- 35
  | HSDLK_DOLLAR    -- 36
  | HSDLK_AMPERSAND -- 38
  | HSDLK_QUOTE     -- 39
  | HSDLK_LEFTPAREN -- 40
  | HSDLK_RIGHTPAREN-- 41
  | HSDLK_ASTERISK  -- 42
  | HSDLK_PLUS      -- 43
  | HSDLK_COMMA     -- 44
  | HSDLK_MINUS     -- 45
  | HSDLK_PERIOD    -- 46
  | HSDLK_SLASH     -- 47
  | HSDLK_0         -- 48
  | HSDLK_1         -- 49
  | HSDLK_2         -- 50
  | HSDLK_3         -- 51
  | HSDLK_4         -- 52
  | HSDLK_5         -- 53
  | HSDLK_6         -- 54
  | HSDLK_7         -- 55
  | HSDLK_8         -- 56
  | HSDLK_9         -- 57
  | HSDLK_COLON     -- 58
  | HSDLK_SEMICOLON -- 59
  | HSDLK_LESS      -- 60
  | HSDLK_EQUALS    -- 61
  | HSDLK_GREATER   -- 62
  | HSDLK_QUESTION  -- 63
  | HSDLK_AT        -- 64
 {- 
   Skip uppercase letters
  -}
  | HSDLK_LEFTBRACKET  -- 91
  | HSDLK_BACKSLASH    -- 92
  | HSDLK_RIGHTBRACKET -- 93
  | HSDLK_CARE         -- 94
  | HSDLK_UNDERSCORE   -- 95
  | HSDLK_BACKQUOTE    -- 96

  | HSDLK_a -- 97
  | HSDLK_b -- 98
  | HSDLK_c -- 99
  | HSDLK_d -- 100
  | HSDLK_e -- 101
  | HSDLK_f -- 102
  | HSDLK_g -- 103
  | HSDLK_h -- 104
  | HSDLK_i -- 105
  | HSDLK_j -- 106
  | HSDLK_k -- 107
  | HSDLK_l -- 108
  | HSDLK_m -- 109
  | HSDLK_n -- 110
  | HSDLK_o -- 111
  | HSDLK_p -- 112
  | HSDLK_q -- 113
  | HSDLK_r -- 114
  | HSDLK_s -- 115
  | HSDLK_t -- 116
  | HSDLK_u -- 117
  | HSDLK_v -- 118
  | HSDLK_w -- 119
  | HSDLK_x -- 120
  | HSDLK_y -- 121
  | HSDLK_z -- 122
  | HSDLK_DELETE -- 127
  -- End of ASCII mapped keysyms

  -- International keyboard syms
  | HSDLK_WORLD_0  -- 160 {- 0xA0 -}
  | HSDLK_WORLD_1  -- 161
  | HSDLK_WORLD_2  -- 162
  | HSDLK_WORLD_3  -- 163
  | HSDLK_WORLD_4  -- 164
  | HSDLK_WORLD_5  -- 165
  | HSDLK_WORLD_6  -- 166
  | HSDLK_WORLD_7  -- 167
  | HSDLK_WORLD_8  -- 168
  | HSDLK_WORLD_9  -- 169
  | HSDLK_WORLD_10 -- 170
  | HSDLK_WORLD_11 -- 171
  | HSDLK_WORLD_12 -- 172
  | HSDLK_WORLD_13 -- 173
  | HSDLK_WORLD_14 -- 174
  | HSDLK_WORLD_15 -- 175
  | HSDLK_WORLD_16 -- 176
  | HSDLK_WORLD_17 -- 177
  | HSDLK_WORLD_18 -- 178
  | HSDLK_WORLD_19 -- 179
  | HSDLK_WORLD_20 -- 180
  | HSDLK_WORLD_21 -- 181
  | HSDLK_WORLD_22 -- 182
  | HSDLK_WORLD_23 -- 183
  | HSDLK_WORLD_24 -- 184
  | HSDLK_WORLD_25 -- 185
  | HSDLK_WORLD_26 -- 186
  | HSDLK_WORLD_27 -- 187
  | HSDLK_WORLD_28 -- 188
  | HSDLK_WORLD_29 -- 189
  | HSDLK_WORLD_30 -- 190
  | HSDLK_WORLD_31 -- 191
  | HSDLK_WORLD_32 -- 192
  | HSDLK_WORLD_33 -- 193
  | HSDLK_WORLD_34 -- 194
  | HSDLK_WORLD_35 -- 195
  | HSDLK_WORLD_36 -- 196
  | HSDLK_WORLD_37 -- 197
  | HSDLK_WORLD_38 -- 198
  | HSDLK_WORLD_39 -- 199
  | HSDLK_WORLD_40 -- 200
  | HSDLK_WORLD_41 -- 201
  | HSDLK_WORLD_42 -- 202
  | HSDLK_WORLD_43 -- 203
  | HSDLK_WORLD_44 -- 204
  | HSDLK_WORLD_45 -- 205
  | HSDLK_WORLD_46 -- 206
  | HSDLK_WORLD_47 -- 207
  | HSDLK_WORLD_48 -- 208
  | HSDLK_WORLD_49 -- 209
  | HSDLK_WORLD_50 -- 210
  | HSDLK_WORLD_51 -- 211
  | HSDLK_WORLD_52 -- 212
  | HSDLK_WORLD_53 -- 213
  | HSDLK_WORLD_54 -- 214
  | HSDLK_WORLD_55 -- 215
  | HSDLK_WORLD_56 -- 216
  | HSDLK_WORLD_57 -- 217
  | HSDLK_WORLD_58 -- 218
  | HSDLK_WORLD_59 -- 219
  | HSDLK_WORLD_60 -- 220
  | HSDLK_WORLD_61 -- 221
  | HSDLK_WORLD_62 -- 222
  | HSDLK_WORLD_63 -- 223
  | HSDLK_WORLD_64 -- 224
  | HSDLK_WORLD_65 -- 225
  | HSDLK_WORLD_66 -- 226
  | HSDLK_WORLD_67 -- 227
  | HSDLK_WORLD_68 -- 228
  | HSDLK_WORLD_69 -- 229
  | HSDLK_WORLD_70 -- 230
  | HSDLK_WORLD_71 -- 231
  | HSDLK_WORLD_72 -- 232
  | HSDLK_WORLD_73 -- 233
  | HSDLK_WORLD_74 -- 234
  | HSDLK_WORLD_75 -- 235
  | HSDLK_WORLD_76 -- 236
  | HSDLK_WORLD_77 -- 237
  | HSDLK_WORLD_78 -- 238
  | HSDLK_WORLD_79 -- 239
  | HSDLK_WORLD_80 -- 240
  | HSDLK_WORLD_81 -- 241
  | HSDLK_WORLD_82 -- 242
  | HSDLK_WORLD_83 -- 243
  | HSDLK_WORLD_84 -- 244
  | HSDLK_WORLD_85 -- 245
  | HSDLK_WORLD_86 -- 246
  | HSDLK_WORLD_87 -- 247
  | HSDLK_WORLD_88 -- 248
  | HSDLK_WORLD_89 -- 249
  | HSDLK_WORLD_90 -- 250
  | HSDLK_WORLD_91 -- 251
  | HSDLK_WORLD_92 -- 252
  | HSDLK_WORLD_93 -- 253
  | HSDLK_WORLD_94 -- 254
  | HSDLK_WORLD_95 -- 255 {- 0xFF -}

  -- Numeric keypad
  | HSDLK_KP0 -- 256
  | HSDLK_KP1 -- 257
  | HSDLK_KP2 -- 258
  | HSDLK_KP3 -- 259
  | HSDLK_KP4 -- 260
  | HSDLK_KP5 -- 261
  | HSDLK_KP6 -- 262
  | HSDLK_KP7 -- 263
  | HSDLK_KP8 -- 264
  | HSDLK_KP9 -- 265
  | HSDLK_KP_PERIOD   -- 266
  | HSDLK_KP_DIVIDE   -- 267
  | HSDLK_KP_MULTIPLY -- 268
  | HSDLK_KP_MINUS    -- 269
  | HSDLK_KP_PLUS     -- 270
  | HSDLK_KP_ENTER    -- 271
  | HSDLK_KP_EQUALS   -- 272

  -- Arrows + Home/End pad
  | HSDLK_UP       -- 273
  | HSDLK_DOWN     -- 274
  | HSDLK_RIGHT    -- 275
  | HSDLK_LEFT     -- 276
  | HSDLK_INSERT   -- 277
  | HSDLK_HOME     -- 278
  | HSDLK_END      -- 279
  | HSDLK_PAGEUP   -- 280
  | HSDLK_PAGEDOWN -- 281

  -- Function keys
  | HSDLK_F1  -- 282
  | HSDLK_F2  -- 283
  | HSDLK_F3  -- 284
  | HSDLK_F4  -- 285
  | HSDLK_F5  -- 286
  | HSDLK_F6  -- 287
  | HSDLK_F7  -- 288
  | HSDLK_F8  -- 289
  | HSDLK_F9  -- 290
  | HSDLK_F10 -- 291
  | HSDLK_F11 -- 292
  | HSDLK_F12 -- 293
  | HSDLK_F13 -- 294
  | HSDLK_F14 -- 295
  | HSDLK_F15 -- 296

  -- Key state modifier keys
  | HSDLK_NUMLOCK   -- 300
  | HSDLK_CAPSLOCK  -- 301
  | HSDLK_SCROLLOCK -- 302
  | HSDLK_RSHIFT    -- 303
  | HSDLK_LSHIFT    -- 304
  | HSDLK_RCTRL     -- 305
  | HSDLK_LCTRL     -- 306
  | HSDLK_RALT      -- 307
  | HSDLK_LALT      -- 308
  | HSDLK_RMETA     -- 309
  | HSDLK_LMETA     -- 310
  | HSDLK_LSUPER    -- 311 -- Left "Windows" key
  | HSDLK_RSUPER    -- 312 -- Right "Windows" key
  | HSDLK_MODE      -- 313 -- "Alt Gr" key
  | HSDLK_COMPOSE   -- 314 -- Multi-key compose key

  -- Miscellaneous function keys
  | HSDLK_HELP   -- 315
  | HSDLK_PRINT  -- 316
  | HSDLK_SYSREQ -- 317
  | HSDLK_BREAK  -- 318
  | HSDLK_MENU   -- 319
  | HSDLK_POWER  -- 320 -- Power Macintosh power key
  | HSDLK_EURO   -- 321 -- Some european keyboards
  | HSDLK_UNDO   -- 322 -- Atari keyboard has Undo

  -- Add any other keys here
  | HSDLK_LAST
  deriving (Eq,Show)

instance Enum SDLKey where
  fromEnum HSDLK_UNKNOWN   = 0
  fromEnum HSDLK_FIRST     = 0
  fromEnum HSDLK_BACKSPACE = 8
  fromEnum HSDLK_TAB       = 9
  fromEnum HSDLK_CLEAR     = 12
  fromEnum HSDLK_RETURN    = 13
  fromEnum HSDLK_PAUSE     = 19
  fromEnum HSDLK_ESCAPE    = 27
  fromEnum HSDLK_SPACE     = 32
  fromEnum HSDLK_EXCLAIM   = 33
  fromEnum HSDLK_QUOTEDBL  = 34
  fromEnum HSDLK_HASH      = 35
  fromEnum HSDLK_DOLLAR    = 36
  fromEnum HSDLK_AMPERSAND = 38
  fromEnum HSDLK_QUOTE     = 39
  fromEnum HSDLK_LEFTPAREN = 40
  fromEnum HSDLK_RIGHTPAREN= 41
  fromEnum HSDLK_ASTERISK  = 42
  fromEnum HSDLK_PLUS      = 43
  fromEnum HSDLK_COMMA     = 44
  fromEnum HSDLK_MINUS     = 45
  fromEnum HSDLK_PERIOD    = 46
  fromEnum HSDLK_SLASH     = 47
  fromEnum HSDLK_0         = 48
  fromEnum HSDLK_1         = 49
  fromEnum HSDLK_2         = 50
  fromEnum HSDLK_3         = 51
  fromEnum HSDLK_4         = 52
  fromEnum HSDLK_5         = 53
  fromEnum HSDLK_6         = 54
  fromEnum HSDLK_7         = 55
  fromEnum HSDLK_8         = 56
  fromEnum HSDLK_9         = 57
  fromEnum HSDLK_COLON     = 58
  fromEnum HSDLK_SEMICOLON = 59
  fromEnum HSDLK_LESS      = 60
  fromEnum HSDLK_EQUALS    = 61
  fromEnum HSDLK_GREATER   = 62
  fromEnum HSDLK_QUESTION  = 63
  fromEnum HSDLK_AT        = 64

  fromEnum HSDLK_LEFTBRACKET  = 91
  fromEnum HSDLK_BACKSLASH    = 92
  fromEnum HSDLK_RIGHTBRACKET = 93
  fromEnum HSDLK_CARE         = 94
  fromEnum HSDLK_UNDERSCORE   = 95
  fromEnum HSDLK_BACKQUOTE    = 96

  fromEnum HSDLK_a = 97
  fromEnum HSDLK_b = 98
  fromEnum HSDLK_c = 99
  fromEnum HSDLK_d = 100
  fromEnum HSDLK_e = 101
  fromEnum HSDLK_f = 102
  fromEnum HSDLK_g = 103
  fromEnum HSDLK_h = 104
  fromEnum HSDLK_i = 105
  fromEnum HSDLK_j = 106
  fromEnum HSDLK_k = 107
  fromEnum HSDLK_l = 108
  fromEnum HSDLK_m = 109
  fromEnum HSDLK_n = 110
  fromEnum HSDLK_o = 111
  fromEnum HSDLK_p = 112
  fromEnum HSDLK_q = 113
  fromEnum HSDLK_r = 114
  fromEnum HSDLK_s = 115
  fromEnum HSDLK_t = 116
  fromEnum HSDLK_u = 117
  fromEnum HSDLK_v = 118
  fromEnum HSDLK_w = 119
  fromEnum HSDLK_x = 120
  fromEnum HSDLK_y = 121
  fromEnum HSDLK_z = 122
  fromEnum HSDLK_DELETE = 127

  fromEnum HSDLK_WORLD_0  = 160
  fromEnum HSDLK_WORLD_1  = 161
  fromEnum HSDLK_WORLD_2  = 162
  fromEnum HSDLK_WORLD_3  = 163
  fromEnum HSDLK_WORLD_4  = 164
  fromEnum HSDLK_WORLD_5  = 165
  fromEnum HSDLK_WORLD_6  = 166
  fromEnum HSDLK_WORLD_7  = 167
  fromEnum HSDLK_WORLD_8  = 168
  fromEnum HSDLK_WORLD_9  = 169
  fromEnum HSDLK_WORLD_10 = 170
  fromEnum HSDLK_WORLD_11 = 171
  fromEnum HSDLK_WORLD_12 = 172
  fromEnum HSDLK_WORLD_13 = 173
  fromEnum HSDLK_WORLD_14 = 174
  fromEnum HSDLK_WORLD_15 = 175
  fromEnum HSDLK_WORLD_16 = 176
  fromEnum HSDLK_WORLD_17 = 177
  fromEnum HSDLK_WORLD_18 = 178
  fromEnum HSDLK_WORLD_19 = 179
  fromEnum HSDLK_WORLD_20 = 180
  fromEnum HSDLK_WORLD_21 = 181
  fromEnum HSDLK_WORLD_22 = 182
  fromEnum HSDLK_WORLD_23 = 183
  fromEnum HSDLK_WORLD_24 = 184
  fromEnum HSDLK_WORLD_25 = 185
  fromEnum HSDLK_WORLD_26 = 186
  fromEnum HSDLK_WORLD_27 = 187
  fromEnum HSDLK_WORLD_28 = 188
  fromEnum HSDLK_WORLD_29 = 189
  fromEnum HSDLK_WORLD_30 = 190
  fromEnum HSDLK_WORLD_31 = 191
  fromEnum HSDLK_WORLD_32 = 192
  fromEnum HSDLK_WORLD_33 = 193
  fromEnum HSDLK_WORLD_34 = 194
  fromEnum HSDLK_WORLD_35 = 195
  fromEnum HSDLK_WORLD_36 = 196
  fromEnum HSDLK_WORLD_37 = 197
  fromEnum HSDLK_WORLD_38 = 198
  fromEnum HSDLK_WORLD_39 = 199
  fromEnum HSDLK_WORLD_40 = 200
  fromEnum HSDLK_WORLD_41 = 201
  fromEnum HSDLK_WORLD_42 = 202
  fromEnum HSDLK_WORLD_43 = 203
  fromEnum HSDLK_WORLD_44 = 204
  fromEnum HSDLK_WORLD_45 = 205
  fromEnum HSDLK_WORLD_46 = 206
  fromEnum HSDLK_WORLD_47 = 207
  fromEnum HSDLK_WORLD_48 = 208
  fromEnum HSDLK_WORLD_49 = 209
  fromEnum HSDLK_WORLD_50 = 210
  fromEnum HSDLK_WORLD_51 = 211
  fromEnum HSDLK_WORLD_52 = 212
  fromEnum HSDLK_WORLD_53 = 213
  fromEnum HSDLK_WORLD_54 = 214
  fromEnum HSDLK_WORLD_55 = 215
  fromEnum HSDLK_WORLD_56 = 216
  fromEnum HSDLK_WORLD_57 = 217
  fromEnum HSDLK_WORLD_58 = 218
  fromEnum HSDLK_WORLD_59 = 219
  fromEnum HSDLK_WORLD_60 = 220
  fromEnum HSDLK_WORLD_61 = 221
  fromEnum HSDLK_WORLD_62 = 222
  fromEnum HSDLK_WORLD_63 = 223
  fromEnum HSDLK_WORLD_64 = 224
  fromEnum HSDLK_WORLD_65 = 225
  fromEnum HSDLK_WORLD_66 = 226
  fromEnum HSDLK_WORLD_67 = 227
  fromEnum HSDLK_WORLD_68 = 228
  fromEnum HSDLK_WORLD_69 = 229
  fromEnum HSDLK_WORLD_70 = 230
  fromEnum HSDLK_WORLD_71 = 231
  fromEnum HSDLK_WORLD_72 = 232
  fromEnum HSDLK_WORLD_73 = 233
  fromEnum HSDLK_WORLD_74 = 234
  fromEnum HSDLK_WORLD_75 = 235
  fromEnum HSDLK_WORLD_76 = 236
  fromEnum HSDLK_WORLD_77 = 237
  fromEnum HSDLK_WORLD_78 = 238
  fromEnum HSDLK_WORLD_79 = 239
  fromEnum HSDLK_WORLD_80 = 240
  fromEnum HSDLK_WORLD_81 = 241
  fromEnum HSDLK_WORLD_82 = 242
  fromEnum HSDLK_WORLD_83 = 243
  fromEnum HSDLK_WORLD_84 = 244
  fromEnum HSDLK_WORLD_85 = 245
  fromEnum HSDLK_WORLD_86 = 246
  fromEnum HSDLK_WORLD_87 = 247
  fromEnum HSDLK_WORLD_88 = 248
  fromEnum HSDLK_WORLD_89 = 249
  fromEnum HSDLK_WORLD_90 = 250
  fromEnum HSDLK_WORLD_91 = 251
  fromEnum HSDLK_WORLD_92 = 252
  fromEnum HSDLK_WORLD_93 = 253
  fromEnum HSDLK_WORLD_94 = 254
  fromEnum HSDLK_WORLD_95 = 255

  fromEnum HSDLK_KP0 = 256
  fromEnum HSDLK_KP1 = 257
  fromEnum HSDLK_KP2 = 258
  fromEnum HSDLK_KP3 = 259
  fromEnum HSDLK_KP4 = 260
  fromEnum HSDLK_KP5 = 261
  fromEnum HSDLK_KP6 = 262
  fromEnum HSDLK_KP7 = 263
  fromEnum HSDLK_KP8 = 264
  fromEnum HSDLK_KP9 = 265
  fromEnum HSDLK_KP_PERIOD   = 266
  fromEnum HSDLK_KP_DIVIDE   = 267
  fromEnum HSDLK_KP_MULTIPLY = 268
  fromEnum HSDLK_KP_MINUS    = 269
  fromEnum HSDLK_KP_PLUS     = 270
  fromEnum HSDLK_KP_ENTER    = 271
  fromEnum HSDLK_KP_EQUALS   = 272

  fromEnum HSDLK_UP       = 273
  fromEnum HSDLK_DOWN     = 274
  fromEnum HSDLK_RIGHT    = 275
  fromEnum HSDLK_LEFT     = 276
  fromEnum HSDLK_INSERT   = 277
  fromEnum HSDLK_HOME     = 278
  fromEnum HSDLK_END      = 279
  fromEnum HSDLK_PAGEUP   = 280
  fromEnum HSDLK_PAGEDOWN = 281

  fromEnum HSDLK_F1  = 282
  fromEnum HSDLK_F2  = 283
  fromEnum HSDLK_F3  = 284
  fromEnum HSDLK_F4  = 285
  fromEnum HSDLK_F5  = 286
  fromEnum HSDLK_F6  = 287
  fromEnum HSDLK_F7  = 288
  fromEnum HSDLK_F8  = 289
  fromEnum HSDLK_F9  = 290
  fromEnum HSDLK_F10 = 291
  fromEnum HSDLK_F11 = 292
  fromEnum HSDLK_F12 = 293
  fromEnum HSDLK_F13 = 294
  fromEnum HSDLK_F14 = 295
  fromEnum HSDLK_F15 = 296

  fromEnum HSDLK_NUMLOCK   = 300
  fromEnum HSDLK_CAPSLOCK  = 301
  fromEnum HSDLK_SCROLLOCK = 302
  fromEnum HSDLK_RSHIFT    = 303
  fromEnum HSDLK_LSHIFT    = 304
  fromEnum HSDLK_RCTRL     = 305
  fromEnum HSDLK_LCTRL     = 306
  fromEnum HSDLK_RALT      = 307
  fromEnum HSDLK_LALT      = 308
  fromEnum HSDLK_RMETA     = 309
  fromEnum HSDLK_LMETA     = 310
  fromEnum HSDLK_LSUPER    = 311
  fromEnum HSDLK_RSUPER    = 312
  fromEnum HSDLK_MODE      = 313
  fromEnum HSDLK_COMPOSE   = 314

  fromEnum HSDLK_HELP   = 315
  fromEnum HSDLK_PRINT  = 316
  fromEnum HSDLK_SYSREQ = 317
  fromEnum HSDLK_BREAK  = 318
  fromEnum HSDLK_MENU   = 319
  fromEnum HSDLK_POWER  = 320
  fromEnum HSDLK_EURO   = 321
  fromEnum HSDLK_UNDO   = 322

  fromEnum HSDLK_LAST   = 323


  toEnum  0 = HSDLK_FIRST     -- 0
  toEnum  8 = HSDLK_BACKSPACE -- 8
  toEnum  9 = HSDLK_TAB       -- 9
  toEnum 12 = HSDLK_CLEAR     -- 12
  toEnum 13 = HSDLK_RETURN    -- 13
  toEnum 19 = HSDLK_PAUSE     -- 19
  toEnum 27 = HSDLK_ESCAPE    -- 27
  toEnum 32 = HSDLK_SPACE     -- 32
  toEnum 33 = HSDLK_EXCLAIM   -- 33
  toEnum 34 = HSDLK_QUOTEDBL  -- 34
  toEnum 35 = HSDLK_HASH      -- 35
  toEnum 36 = HSDLK_DOLLAR    -- 36
  toEnum 38 = HSDLK_AMPERSAND -- 38
  toEnum 39 = HSDLK_QUOTE     -- 39
  toEnum 40 = HSDLK_LEFTPAREN -- 40
  toEnum 41 = HSDLK_RIGHTPAREN-- 41
  toEnum 42 = HSDLK_ASTERISK  -- 42
  toEnum 43 = HSDLK_PLUS      -- 43
  toEnum 44 = HSDLK_COMMA     -- 44
  toEnum 45 = HSDLK_MINUS     -- 45
  toEnum 46 = HSDLK_PERIOD    -- 46
  toEnum 47 = HSDLK_SLASH     -- 47
  toEnum 48 = HSDLK_0         -- 48
  toEnum 49 = HSDLK_1         -- 49
  toEnum 50 = HSDLK_2         -- 50
  toEnum 51 = HSDLK_3         -- 51
  toEnum 52 = HSDLK_4         -- 52
  toEnum 53 = HSDLK_5         -- 53
  toEnum 54 = HSDLK_6         -- 54
  toEnum 55 = HSDLK_7         -- 55
  toEnum 56 = HSDLK_8         -- 56
  toEnum 57 = HSDLK_9         -- 57
  toEnum 58 = HSDLK_COLON     -- 58
  toEnum 59 = HSDLK_SEMICOLON -- 59
  toEnum 60 = HSDLK_LESS      -- 60
  toEnum 61 = HSDLK_EQUALS    -- 61
  toEnum 62 = HSDLK_GREATER   -- 62
  toEnum 63 = HSDLK_QUESTION  -- 63
  toEnum 64 = HSDLK_AT        -- 64

  toEnum 91 = HSDLK_LEFTBRACKET  -- 91
  toEnum 92 = HSDLK_BACKSLASH    -- 92
  toEnum 93 = HSDLK_RIGHTBRACKET -- 93
  toEnum 94 = HSDLK_CARE         -- 94
  toEnum 95 = HSDLK_UNDERSCORE   -- 95
  toEnum 96 = HSDLK_BACKQUOTE    -- 96

  toEnum  97 = HSDLK_a -- 97
  toEnum  98 = HSDLK_b -- 98
  toEnum  99 = HSDLK_c -- 99
  toEnum 100 = HSDLK_d -- 100
  toEnum 101 = HSDLK_e -- 101
  toEnum 102 = HSDLK_f -- 102
  toEnum 103 = HSDLK_g -- 103
  toEnum 104 = HSDLK_h -- 104
  toEnum 105 = HSDLK_i -- 105
  toEnum 106 = HSDLK_j -- 106
  toEnum 107 = HSDLK_k -- 107
  toEnum 108 = HSDLK_l -- 108
  toEnum 109 = HSDLK_m -- 109
  toEnum 110 = HSDLK_n -- 110
  toEnum 111 = HSDLK_o -- 111
  toEnum 112 = HSDLK_p -- 112
  toEnum 113 = HSDLK_q -- 113
  toEnum 114 = HSDLK_r -- 114
  toEnum 115 = HSDLK_s -- 115
  toEnum 116 = HSDLK_t -- 116
  toEnum 117 = HSDLK_u -- 117
  toEnum 118 = HSDLK_v -- 118
  toEnum 119 = HSDLK_w -- 119
  toEnum 120 = HSDLK_x -- 120
  toEnum 121 = HSDLK_y -- 121
  toEnum 122 = HSDLK_z -- 122
  toEnum 127 = HSDLK_DELETE -- 127

  toEnum 160 = HSDLK_WORLD_0  -- 160
  toEnum 161 = HSDLK_WORLD_1  -- 161
  toEnum 162 = HSDLK_WORLD_2  -- 162
  toEnum 163 = HSDLK_WORLD_3  -- 163
  toEnum 164 = HSDLK_WORLD_4  -- 164
  toEnum 165 = HSDLK_WORLD_5  -- 165
  toEnum 166 = HSDLK_WORLD_6  -- 166
  toEnum 167 = HSDLK_WORLD_7  -- 167
  toEnum 168 = HSDLK_WORLD_8  -- 168
  toEnum 169 = HSDLK_WORLD_9  -- 169
  toEnum 170 = HSDLK_WORLD_10 -- 170
  toEnum 171 = HSDLK_WORLD_11 -- 171
  toEnum 172 = HSDLK_WORLD_12 -- 172
  toEnum 173 = HSDLK_WORLD_13 -- 173
  toEnum 174 = HSDLK_WORLD_14 -- 174
  toEnum 175 = HSDLK_WORLD_15 -- 175
  toEnum 176 = HSDLK_WORLD_16 -- 176
  toEnum 177 = HSDLK_WORLD_17 -- 177
  toEnum 178 = HSDLK_WORLD_18 -- 178
  toEnum 179 = HSDLK_WORLD_19 -- 179
  toEnum 180 = HSDLK_WORLD_20 -- 180
  toEnum 181 = HSDLK_WORLD_21 -- 181
  toEnum 182 = HSDLK_WORLD_22 -- 182
  toEnum 183 = HSDLK_WORLD_23 -- 183
  toEnum 184 = HSDLK_WORLD_24 -- 184
  toEnum 185 = HSDLK_WORLD_25 -- 185
  toEnum 186 = HSDLK_WORLD_26 -- 186
  toEnum 187 = HSDLK_WORLD_27 -- 187
  toEnum 188 = HSDLK_WORLD_28 -- 188
  toEnum 189 = HSDLK_WORLD_29 -- 189
  toEnum 190 = HSDLK_WORLD_30 -- 190
  toEnum 191 = HSDLK_WORLD_31 -- 191
  toEnum 192 = HSDLK_WORLD_32 -- 192
  toEnum 193 = HSDLK_WORLD_33 -- 193
  toEnum 194 = HSDLK_WORLD_34 -- 194
  toEnum 195 = HSDLK_WORLD_35 -- 195
  toEnum 196 = HSDLK_WORLD_36 -- 196
  toEnum 197 = HSDLK_WORLD_37 -- 197
  toEnum 198 = HSDLK_WORLD_38 -- 198
  toEnum 199 = HSDLK_WORLD_39 -- 199
  toEnum 200 = HSDLK_WORLD_40 -- 200
  toEnum 201 = HSDLK_WORLD_41 -- 201
  toEnum 202 = HSDLK_WORLD_42 -- 202
  toEnum 203 = HSDLK_WORLD_43 -- 203
  toEnum 204 = HSDLK_WORLD_44 -- 204
  toEnum 205 = HSDLK_WORLD_45 -- 205
  toEnum 206 = HSDLK_WORLD_46 -- 206
  toEnum 207 = HSDLK_WORLD_47 -- 207
  toEnum 208 = HSDLK_WORLD_48 -- 208
  toEnum 209 = HSDLK_WORLD_49 -- 209
  toEnum 210 = HSDLK_WORLD_50 -- 210
  toEnum 211 = HSDLK_WORLD_51 -- 211
  toEnum 212 = HSDLK_WORLD_52 -- 212
  toEnum 213 = HSDLK_WORLD_53 -- 213
  toEnum 214 = HSDLK_WORLD_54 -- 214
  toEnum 215 = HSDLK_WORLD_55 -- 215
  toEnum 216 = HSDLK_WORLD_56 -- 216
  toEnum 217 = HSDLK_WORLD_57 -- 217
  toEnum 218 = HSDLK_WORLD_58 -- 218
  toEnum 219 = HSDLK_WORLD_59 -- 219
  toEnum 220 = HSDLK_WORLD_60 -- 220
  toEnum 221 = HSDLK_WORLD_61 -- 221
  toEnum 222 = HSDLK_WORLD_62 -- 222
  toEnum 223 = HSDLK_WORLD_63 -- 223
  toEnum 224 = HSDLK_WORLD_64 -- 224
  toEnum 225 = HSDLK_WORLD_65 -- 225
  toEnum 226 = HSDLK_WORLD_66 -- 226
  toEnum 227 = HSDLK_WORLD_67 -- 227
  toEnum 228 = HSDLK_WORLD_68 -- 228
  toEnum 229 = HSDLK_WORLD_69 -- 229
  toEnum 230 = HSDLK_WORLD_70 -- 230
  toEnum 231 = HSDLK_WORLD_71 -- 231
  toEnum 232 = HSDLK_WORLD_72 -- 232
  toEnum 233 = HSDLK_WORLD_73 -- 233
  toEnum 234 = HSDLK_WORLD_74 -- 234
  toEnum 235 = HSDLK_WORLD_75 -- 235
  toEnum 236 = HSDLK_WORLD_76 -- 236
  toEnum 237 = HSDLK_WORLD_77 -- 237
  toEnum 238 = HSDLK_WORLD_78 -- 238
  toEnum 239 = HSDLK_WORLD_79 -- 239
  toEnum 240 = HSDLK_WORLD_80 -- 240
  toEnum 241 = HSDLK_WORLD_81 -- 241
  toEnum 242 = HSDLK_WORLD_82 -- 242
  toEnum 243 = HSDLK_WORLD_83 -- 243
  toEnum 244 = HSDLK_WORLD_84 -- 244
  toEnum 245 = HSDLK_WORLD_85 -- 245
  toEnum 246 = HSDLK_WORLD_86 -- 246
  toEnum 247 = HSDLK_WORLD_87 -- 247
  toEnum 248 = HSDLK_WORLD_88 -- 248
  toEnum 249 = HSDLK_WORLD_89 -- 249
  toEnum 250 = HSDLK_WORLD_90 -- 250
  toEnum 251 = HSDLK_WORLD_91 -- 251
  toEnum 252 = HSDLK_WORLD_92 -- 252
  toEnum 253 = HSDLK_WORLD_93 -- 253
  toEnum 254 = HSDLK_WORLD_94 -- 254
  toEnum 255 = HSDLK_WORLD_95 -- 255

  toEnum 256 = HSDLK_KP0 -- 256
  toEnum 257 = HSDLK_KP1 -- 257
  toEnum 258 = HSDLK_KP2 -- 258
  toEnum 259 = HSDLK_KP3 -- 259
  toEnum 260 = HSDLK_KP4 -- 260
  toEnum 261 = HSDLK_KP5 -- 261
  toEnum 262 = HSDLK_KP6 -- 262
  toEnum 263 = HSDLK_KP7 -- 263
  toEnum 264 = HSDLK_KP8 -- 264
  toEnum 265 = HSDLK_KP9 -- 265
  toEnum 266 = HSDLK_KP_PERIOD   -- 266
  toEnum 267 = HSDLK_KP_DIVIDE   -- 267
  toEnum 268 = HSDLK_KP_MULTIPLY -- 268
  toEnum 269 = HSDLK_KP_MINUS    -- 269
  toEnum 270 = HSDLK_KP_PLUS     -- 270
  toEnum 271 = HSDLK_KP_ENTER    -- 271
  toEnum 272 = HSDLK_KP_EQUALS   -- 272

  toEnum 273 = HSDLK_UP       -- 273
  toEnum 274 = HSDLK_DOWN     -- 274
  toEnum 275 = HSDLK_RIGHT    -- 275
  toEnum 276 = HSDLK_LEFT     -- 276
  toEnum 277 = HSDLK_INSERT   -- 277
  toEnum 278 = HSDLK_HOME     -- 278
  toEnum 279 = HSDLK_END      -- 279
  toEnum 280 = HSDLK_PAGEUP   -- 280
  toEnum 281 = HSDLK_PAGEDOWN -- 281

  toEnum 282 = HSDLK_F1  -- 282
  toEnum 283 = HSDLK_F2  -- 283
  toEnum 284 = HSDLK_F3  -- 284
  toEnum 285 = HSDLK_F4  -- 285
  toEnum 286 = HSDLK_F5  -- 286
  toEnum 287 = HSDLK_F6  -- 287
  toEnum 288 = HSDLK_F7  -- 288
  toEnum 289 = HSDLK_F8  -- 289
  toEnum 290 = HSDLK_F9  -- 290
  toEnum 291 = HSDLK_F10 -- 291
  toEnum 292 = HSDLK_F11 -- 292
  toEnum 293 = HSDLK_F12 -- 293
  toEnum 294 = HSDLK_F13 -- 294
  toEnum 295 = HSDLK_F14 -- 295
  toEnum 296 = HSDLK_F15 -- 296

  toEnum 300 = HSDLK_NUMLOCK   -- 300
  toEnum 301 = HSDLK_CAPSLOCK  -- 301
  toEnum 302 = HSDLK_SCROLLOCK -- 302
  toEnum 303 = HSDLK_RSHIFT    -- 303
  toEnum 304 = HSDLK_LSHIFT    -- 304
  toEnum 305 = HSDLK_RCTRL     -- 305
  toEnum 306 = HSDLK_LCTRL     -- 306
  toEnum 307 = HSDLK_RALT      -- 307
  toEnum 308 = HSDLK_LALT      -- 308
  toEnum 309 = HSDLK_RMETA     -- 309
  toEnum 310 = HSDLK_LMETA     -- 310
  toEnum 311 = HSDLK_LSUPER    -- 311
  toEnum 312 = HSDLK_RSUPER    -- 312
  toEnum 313 = HSDLK_MODE      -- 313
  toEnum 314 = HSDLK_COMPOSE   -- 314

  toEnum 315 = HSDLK_HELP   -- 315
  toEnum 316 = HSDLK_PRINT  -- 316
  toEnum 317 = HSDLK_SYSREQ -- 317
  toEnum 318 = HSDLK_BREAK  -- 318
  toEnum 319 = HSDLK_MENU   -- 319
  toEnum 320 = HSDLK_POWER  -- 320
  toEnum 321 = HSDLK_EURO   -- 321
  toEnum 322 = HSDLK_UNDO   -- 322

  toEnum 323 = HSDLK_LAST   -- 323
  toEnum _   = HSDLK_UNKNOWN

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
