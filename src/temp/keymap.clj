
(ns temp.core)

(import org.lwjgl.glfw.GLFW)

(def keymap {GLFW/GLFW_KEY_SPACE                   nil
             GLFW/GLFW_KEY_APOSTROPHE              nil
             GLFW/GLFW_KEY_COMMA                   ","
             GLFW/GLFW_KEY_MINUS                   nil
             GLFW/GLFW_KEY_PERIOD                  "."
             GLFW/GLFW_KEY_SLASH                   nil
             GLFW/GLFW_KEY_0                       "0"
             GLFW/GLFW_KEY_1                       "1"
             GLFW/GLFW_KEY_2                       "2"
             GLFW/GLFW_KEY_3                       "3"
             GLFW/GLFW_KEY_4                       "4"
             GLFW/GLFW_KEY_5                       "5"
             GLFW/GLFW_KEY_6                       "6"
             GLFW/GLFW_KEY_7                       "7"
             GLFW/GLFW_KEY_8                       "8"
             GLFW/GLFW_KEY_9                       "9"
             GLFW/GLFW_KEY_SEMICOLON               nil
             GLFW/GLFW_KEY_EQUAL                   nil
             GLFW/GLFW_KEY_A                       "a"
             GLFW/GLFW_KEY_B                       "b"
             GLFW/GLFW_KEY_C                       "c"
             GLFW/GLFW_KEY_D                       "d"
             GLFW/GLFW_KEY_E                       "e"
             GLFW/GLFW_KEY_F                       "f"
             GLFW/GLFW_KEY_G                       "g"
             GLFW/GLFW_KEY_H                       "h"
             GLFW/GLFW_KEY_I                       "i"
             GLFW/GLFW_KEY_J                       "j"
             GLFW/GLFW_KEY_K                       "k"
             GLFW/GLFW_KEY_L                       "l"
             GLFW/GLFW_KEY_M                       "m"
             GLFW/GLFW_KEY_N                       "n"
             GLFW/GLFW_KEY_O                       "o"
             GLFW/GLFW_KEY_P                       "p"
             GLFW/GLFW_KEY_Q                       "q"
             GLFW/GLFW_KEY_R                       "r"
             GLFW/GLFW_KEY_S                       "s"
             GLFW/GLFW_KEY_T                       "t"
             GLFW/GLFW_KEY_U                       "u"
             GLFW/GLFW_KEY_V                       "v"
             GLFW/GLFW_KEY_W                       "w"
             GLFW/GLFW_KEY_X                       "x"
             GLFW/GLFW_KEY_Y                       "y"
             GLFW/GLFW_KEY_Z                       "z"
             GLFW/GLFW_KEY_LEFT_BRACKET            nil
             GLFW/GLFW_KEY_BACKSLASH               nil
             GLFW/GLFW_KEY_RIGHT_BRACKET           nil
             GLFW/GLFW_KEY_ESCAPE                  "esc"
             GLFW/GLFW_KEY_ENTER                   :enter
             GLFW/GLFW_KEY_TAB                     nil
             GLFW/GLFW_KEY_BACKSPACE               nil
             GLFW/GLFW_KEY_INSERT                  nil
             GLFW/GLFW_KEY_DELETE                  nil
             GLFW/GLFW_KEY_RIGHT                   nil
             GLFW/GLFW_KEY_LEFT                    nil
             GLFW/GLFW_KEY_DOWN                    nil
             GLFW/GLFW_KEY_UP                      nil
             GLFW/GLFW_KEY_PAGE_UP                 nil
             GLFW/GLFW_KEY_PAGE_DOWN               nil
             GLFW/GLFW_KEY_HOME                    nil
             GLFW/GLFW_KEY_END                     nil
             GLFW/GLFW_KEY_F1                      nil
             GLFW/GLFW_KEY_F2                      nil
             GLFW/GLFW_KEY_F3                      nil
             GLFW/GLFW_KEY_F4                      nil
             GLFW/GLFW_KEY_F5                      nil
             GLFW/GLFW_KEY_F6                      nil
             GLFW/GLFW_KEY_F7                      nil
             GLFW/GLFW_KEY_F8                      nil
             GLFW/GLFW_KEY_F9                      nil
             GLFW/GLFW_KEY_F10                     nil
             GLFW/GLFW_KEY_F11                     nil
             GLFW/GLFW_KEY_F12                     nil
             GLFW/GLFW_KEY_KP_DECIMAL              nil
             GLFW/GLFW_KEY_KP_DIVIDE               nil
             GLFW/GLFW_KEY_KP_MULTIPLY             nil
             GLFW/GLFW_KEY_KP_SUBTRACT             nil
             GLFW/GLFW_KEY_KP_ADD                  nil
             GLFW/GLFW_KEY_KP_ENTER                nil
             GLFW/GLFW_KEY_KP_EQUAL                nil
             })

