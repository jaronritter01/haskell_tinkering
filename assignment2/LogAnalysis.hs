module LogAnalysis where

import Data.Char (digitToInt)
import Log ( LogMessage(..), MessageTree(..), MessageType(..) )

parseFirstNum :: String -> String
parseFirstNum (' ' : rest)   = ""
parseFirstNum (first : rest) = first : parseFirstNum rest

getNextToken :: String -> String
getNextToken (' ' : rest)   = rest
getNextToken (first : rest) = getNextToken rest

parseMessageType :: String -> Maybe MessageType
parseMessageType ('I' : ' ' : _) = Just Info
parseMessageType ('W' : ' ' : _) = Just Warning
parseMessageType ('E' : ' ' : n) = Just (Error (read (parseFirstNum n)))
parseMessageType _               = Nothing

parseMessage :: String -> LogMessage
parseMessage line = do
  let posMsgTyp = parseMessageType line
  case posMsgTyp of
    Just value ->
      if value == Info || value == Warning
        then LogMessage value (read (parseFirstNum (getNextToken line))) (getNextToken (getNextToken line))
        else LogMessage value (read (parseFirstNum (getNextToken (getNextToken line)))) (getNextToken (getNextToken (getNextToken line)))
    Nothing -> Unknown line

parse :: String -> [LogMessage]
parse fileContents = map parseMessage (lines fileContents)

getTimestamp :: LogMessage -> Int
getTimestamp (LogMessage _ timestamp _) = timestamp

getRightNode :: MessageTree -> MessageTree
getRightNode (Node _ _ rightNode) = rightNode
getRightNode Leaf                 = Leaf

getLeftNode :: MessageTree -> MessageTree
getLeftNode (Node leftNode _ _) = leftNode
getLeftNode Leaf                = Leaf

getCurrentNode :: MessageTree -> LogMessage
getCurrentNode (Node _ currentNode _) = currentNode

insert :: LogMessage -> MessageTree -> MessageTree
insert newNode tree =
    case newNode of
      Unknown _ -> tree
      LogMessage _ newTimeStamp _ -> 
        let currentValue :: Int = getTimestamp (getCurrentNode tree)
        in
          case tree of -- If the tree is just a leaf node
            Leaf -> Node Leaf newNode Leaf
            Node leftNode currentMessage rightNode -> -- If the tree is not just a node
              if newTimeStamp < getTimestamp (getCurrentNode tree)
                then case leftNode of
                  Leaf -> Node (Node Leaf newNode Leaf) (getCurrentNode tree) rightNode
                  Node leftTree message rightTree -> Node (insert newNode leftNode) (getCurrentNode tree) rightNode
                else case rightNode of
                  Leaf -> Node leftNode (getCurrentNode tree) (Node Leaf newNode Leaf)
                  Node leftTree message rightTree -> Node rightNode (getCurrentNode tree) (insert newNode rightNode)

main :: IO ()
main = do
  let log =
        "\
        \I 5053 pci_id: con ing! \n\
        \I 4681 ehci 0xf43d000:15: regista14: [0xbffff 0xfed nosabled 00-02] Zonseres: brips byted nored) \n\
        \W 3654 e8] PGTT ASF! 00f00000003.2: 0x000 - 0000: 00009dbfffec00000: Pround/f1743colled \n\
        \I 4076 verse.' \n\
        \I 4764 He trusts to you to set them free, \n\
        \I 858 your pocket?' he went on, turning to Alice. \n\
        \I 898 would be offended again. \n\
        \I 3753 pci 0x18fff steresocared, overne: 0000 (le wailan0: ressio0/derveld fory: alinpu-all) \n\
        \I 790 those long words, and, what's more, I don't believe you do either!' And \n\
        \I 3899 hastily.\n"
      messageOne = parseMessage "I 5053 pci_id: con ing!"
      messageTwo = parseMessage " 4681 ehci 0xf43d000:15: regista14: [0xbffff 0xfed nosabled 00-02] Zonseres: brips byted nored)"
      messageThree = parseMessage "3654 e8] PGTT ASF! 00f00000003.2: 0x000 - 0000: 00009dbfffec00000: Pround/f1743colled"
      startTree :: MessageTree = Leaf
      newTree = insert messageOne startTree
  print newTree