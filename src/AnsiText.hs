module AnsiText where

import           Data.Char

-- http://www.roysac.com/thedrawfonts-tdf.html

charSP :: [String]
charSP = ["    ",
          "    ",
          "    ",
          "    ",
          "    ",
          "    ",
          "    "]

char0 :: [String]
char0 = ["         ",
         " ██████╗ ",
         "██╔═████╗",
         "██║██╔██║",
         "████╔╝██║",
         "╚██████╔╝",
         " ╚═════╝ "]

char1 :: [String]
char1 = ["    ",
         " ██╗",
         "███║",
         "╚██║",
         " ██║",
         " ██║",
         " ╚═╝"]

char2 :: [String]
char2 = ["        ",
         "██████╗ ",
         "╚════██╗",
         " █████╔╝",
         "██╔═══╝ ",
         "███████╗",
         "╚══════╝"]

char3 :: [String]
char3 = ["        ",
         "██████╗ ",
         "╚════██╗",
         " █████╔╝",
         " ╚═══██╗",
         "██████╔╝",
         "╚═════╝ "]

char4 :: [String]
char4 = ["        ",
         "██╗  ██╗",
         "██║  ██║",
         "███████║",
         "╚════██║",
         "     ██║",
         "     ╚═╝"]

char5 :: [String]
char5 = ["        ",
         "███████╗",
         "██╔════╝",
         "███████╗",
         "╚════██║",
         "███████║",
         "╚══════╝"]

char6 :: [String]
char6 = ["        ",
         " ██████╗ ",
         "██╔════╝ ",
         "███████╗ ",
         "██╔═══██╗",
         "╚██████╔╝",
         " ╚═════╝ "]

char7 :: [String]
char7 = ["        ",
         "███████╗",
         "╚════██║",
         "    ██╔╝",
         "   ██╔╝ ",
         "   ██║  ",
         "   ╚═╝  "]

char8 :: [String]
char8 = ["        ",
         " █████╗ ",
         "██╔══██╗",
         "╚█████╔╝",
         "██╔══██╗",
         "╚█████╔╝",
         " ╚════╝ "]

char9 :: [String]
char9 = ["        ",
         " █████╗ ",
         "██╔══██╗",
         "╚██████║",
         " ╚═══██║",
         " █████╔╝",
         " ╚════╝ "]

charA :: [String]
charA = ["        ",
         " █████╗ ",
         "██╔══██╗",
         "███████║",
         "██╔══██║",
         "██║  ██║",
         "╚═╝  ╚═╝"]

charB :: [String]
charB = ["        ",
         "██████╗ ",
         "██╔══██╗",
         "██████╔╝",
         "██╔══██╗",
         "██████╔╝",
         "╚═════╝ "]

charC :: [String]
charC = ["        ",
         " ██████╗",
         "██╔════╝",
         "██║     ",
         "██║     ",
         "╚██████╗",
         " ╚═════╝"]

charD :: [String]
charD = ["        ",
         "██████╗ ",
         "██╔══██╗",
         "██║  ██║",
         "██║  ██║",
         "██████╔╝",
         "╚═════╝ "]

charE :: [String]
charE = ["        ",
         "███████╗",
         "██╔════╝",
         "█████╗  ",
         "██╔══╝  ",
         "███████╗",
         "╚══════╝"]

charF :: [String]
charF = ["        ",
         "███████╗",
         "██╔════╝",
         "█████╗  ",
         "██╔══╝  ",
         "██║     ",
         "╚═╝     "]

charG :: [String]
charG = ["        ",
         " ██████╗ ",
         "██╔════╝ ",
         "██║  ███╗",
         "██║   ██║",
         "╚██████╔╝",
         " ╚═════╝ "]

charH :: [String]
charH = ["        ",
         "██╗  ██╗",
         "██║  ██║",
         "███████║",
         "██╔══██║",
         "██║  ██║",
         "╚═╝  ╚═╝"]

charI :: [String]
charI = ["   ",
         "██╗",
         "██║",
         "██║",
         "██║",
         "██║",
         "╚═╝"]

charJ :: [String]
charJ = ["        ",
         "     ██╗",
         "     ██║",
         "     ██║",
         "██   ██║",
         "╚█████╔╝",
         " ╚════╝ "]

charK :: [String]
charK = ["        ",
         "██╗  ██╗",
         "██║ ██╔╝",
         "█████╔╝ ",
         "██╔═██╗ ",
         "██║  ██╗",
         "╚═╝  ╚═╝"]

charL :: [String]
charL = ["        ",
         "██╗     ",
         "██║     ",
         "██║     ",
         "██║     ",
         "███████╗",
         "╚══════╝"]

charM :: [String]
charM = ["           ",
         "███╗   ███╗",
         "████╗ ████║",
         "██╔████╔██║",
         "██║╚██╔╝██║",
         "██║ ╚═╝ ██║",
         "╚═╝     ╚═╝"]

charN :: [String]
charN = ["          ",
         "███╗   ██╗",
         "████╗  ██║",
         "██╔██╗ ██║",
         "██║╚██╗██║",
         "██║ ╚████║",
         "╚═╝  ╚═══╝"]

charO :: [String]
charO = ["         ",
         " ██████╗ ",
         "██╔═══██╗",
         "██║   ██║",
         "██║   ██║",
         "╚██████╔╝",
         " ╚═════╝ "]

charP :: [String]
charP = ["        ",
         "██████╗ ",
         "██╔══██╗",
         "██████╔╝",
         "██╔═══╝ ",
         "██║     ",
         "╚═╝     "]

charQ :: [String]
charQ = ["         ",
         " ██████╗ ",
         "██╔═══██╗",
         "██║   ██║",
         "██║▄▄ ██║",
         "╚██████╔╝",
         " ╚══▀▀═╝ "]

charR :: [String]
charR = ["        ",
         "██████╗ ",
         "██╔══██╗",
         "██████╔╝",
         "██╔══██╗",
         "██║  ██║",
         "╚═╝  ╚═╝"]

charS :: [String]
charS = ["        ",
         "███████╗",
         "██╔════╝",
         "███████╗",
         "╚════██║",
         "███████║",
         "╚══════╝"]

charT :: [String]
charT = ["         ",
         "████████╗",
         "╚══██╔══╝",
         "   ██║   ",
         "   ██║   ",
         "   ██║   ",
         "   ╚═╝   "]

charU :: [String]
charU = ["         ",
         "██╗   ██╗",
         "██║   ██║",
         "██║   ██║",
         "██║   ██║",
         "╚██████╔╝",
         " ╚═════╝ "]

charV :: [String]
charV = ["         ",
         "██╗   ██╗",
         "██║   ██║",
         "██║   ██║",
         "╚██╗ ██╔╝",
         " ╚████╔╝ ",
         "  ╚═══╝  "]

charW :: [String]
charW = ["          ",
         "██╗    ██╗",
         "██║    ██║",
         "██║ █╗ ██║",
         "██║███╗██║",
         "╚███╔███╔╝",
         " ╚══╝╚══╝ "]

charX :: [String]
charX = ["        ",
         "██╗  ██╗",
         "╚██╗██╔╝",
         " ╚███╔╝ ",
         " ██╔██╗ ",
         "██╔╝ ██╗",
         "╚═╝  ╚═╝"]

charY :: [String]
charY = ["         ",
         "██╗   ██╗",
         "╚██╗ ██╔╝",
         " ╚████╔╝ ",
         "  ╚██╔╝  ",
         "   ██║   ",
         "   ╚═╝   "]

charZ :: [String]
charZ = ["        ",
         "███████╗",
         "╚══███╔╝",
         "  ███╔╝ ",
         " ███╔╝  ",
         "███████╗",
         "╚══════╝"]

charAMP :: [String]
charAMP = ["         ",
           "   ██╗   ",
           "   ██║   ",
           "████████╗",
           "██╔═██╔═╝",
           "██████║  ",
           "╚═════╝  "]

charSHA :: [String]
charSHA = ["         ",
           " ██╗ ██╗ ",
           "████████╗",
           "╚██╔═██╔╝",
           "████████╗",
           "╚██╔═██╔╝",
           " ╚═╝ ╚═╝ "]

charLB :: [String]
charLB = ["    ",
          " ██╗",
          "██╔╝",
          "██║ ",
          "██║ ",
          "╚██╗",
          " ╚═╝"]

charLSB :: [String]
charLSB = ["    ",
           "███╗",
           "██╔╝",
           "██║ ",
           "██║ ",
           "███╗",
           "╚══╝"]

charDASH :: [String]
charDASH = ["      ",
            "      ",
            "      ",
            "█████╗",
            "╚════╝",
            "      ",
            "      "]

charUNDER :: [String]
charUNDER = ["        ",
             "        ",
             "        ",
             "        ",
             "        ",
             "███████╗",
             "╚══════╝"]

charCIRC :: [String]
charCIRC = ["      ",
            " ███╗ ",
            "██╔██╗",
            "╚═╝╚═╝",
            "      ",
            "      ",
            "      "]

charATSIG :: [String]
charATSIG = ["         ",
             " ██████╗ ",
             "██╔═══██╗",
             "██║██╗██║",
             "██║██║██║",
             "╚█║████╔╝",
             " ╚╝╚═══╝ "]

charRB :: [String]
charRB = ["    ",
          "██╗ ",
          "╚██╗",
          " ██║",
          " ██║",
          "██╔╝",
          "╚═╝ "]

charRSB :: [String]
charRSB = ["    ",
           "███╗",
           "╚██║",
           " ██║",
           " ██║",
           "███║",
           "╚══╝"]

charGT :: [String]
charGT = ["     ",
          "██╗  ",
          "╚██╗ ",
          " ╚██╗",
          " ██╔╝",
          "██╔╝ ",
          "╚═╝  "]

charLT :: [String]
charLT = ["     ",
          "  ██╗",
          " ██╔╝",
          "██╔╝ ",
          "╚██╗ ",
          " ╚██╗",
          "  ╚═╝"]

charQUEST :: [String]
charQUEST = ["         ",
             "██████╗  ",
             "╚════██╗ ",
             "  ▄███╔╝ ",
             "  ▀▀══╝  ",
             "  ██╗    ",
             "  ╚═╝    "]

charCOMMA :: [String]
charCOMMA = ["    ",
             "    ",
             "    ",
             "    ",
             "    ",
             "▄█╗ ",
             "╚═╝ "]

charDOT :: [String]
charDOT = ["   ",
           "   ",
           "   ",
           "   ",
           "   ",
           "██╗",
           "╚═╝"]

charSEMIC :: [String]
charSEMIC = ["    ",
             "    ",
             "    ",
             "██╗ ",
             "╚═╝ ",
             "▄█╗ ",
             "▀═╝ "]

charSLASH :: [String]
charSLASH = ["       ",
             "    ██╗",
             "   ██╔╝",
             "  ██╔╝ ",
             " ██╔╝  ",
             "██╔╝   ",
             "╚═╝    "]

charCOLON :: [String]
charCOLON = ["    ",
             "    ",
             "██╗ ",
             "╚═╝ ",
             "██╗ ",
             "╚═╝ ",
             "    "]

charEXCLAM :: [String]
charEXCLAM = ["      ",
              "  ██╗ ",
              "  ██║ ",
              "  ██║ ",
              "  ╚═╝ ",
              "  ██╗ ",
              "  ╚═╝ "]

-- " ! #  & ()  ,-./0123456789:;< >?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[ ]^_"
-- " !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_"
charset :: [[String]]
charset = [
    charSP,    charEXCLAM, charSP,    charSHA,   charSP,   charSP,
    charAMP,   charSP,     charLB,    charRB,    charSP,   charSP,
    charCOMMA, charDASH,   charDOT,   charSLASH, char0,    char1,
    char2,     char3,      char4,     char5,     char6,    char7,
    char8,     char9,      charCOLON, charSEMIC, charLT,   charSP,
    charGT,    charQUEST,  charATSIG, charA,     charB,    charC,
    charD,     charE,      charF,     charG,     charH,    charI,
    charJ,     charK,      charL,     charM,     charN,    charO,
    charP,     charQ,      charR,     charS,     charT,    charU,
    charV,     charW,      charX,     charY,     charZ,    charLSB,
    charSP,    charRSB,    charCIRC,  charUNDER]

makeAnsiText :: String -> String
makeAnsiText str = unlines $ (\line-> unwords $ (\char-> charset!!((if ord char > 95 then ord ' ' else ord char) - ord ' ')!!line) . toUpper <$> str) <$> [0..6]
