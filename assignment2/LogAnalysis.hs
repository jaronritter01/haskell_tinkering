module LogAnalysis where

import Data.Char (digitToInt)
import Log (LogMessage (..), MessageTree (..), MessageType (..))

parseFirstNum :: String -> String
parseFirstNum (' ' : rest) = ""
parseFirstNum (first : "") = [first]
parseFirstNum (first : rest) = first : parseFirstNum rest

getNextToken :: String -> String
getNextToken [] = []
getNextToken (' ' : rest) = rest
getNextToken (first : rest) = getNextToken rest

parseMessageType :: String -> Maybe MessageType
parseMessageType ('I' : ' ' : _) = Just Info
parseMessageType ('W' : ' ' : _) = Just Warning
parseMessageType ('E' : ' ' : n) = Just (Error (read (parseFirstNum n)))
parseMessageType _ = Nothing

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
getRightNode Leaf = Leaf

getLeftNode :: MessageTree -> MessageTree
getLeftNode (Node leftNode _ _) = leftNode
getLeftNode Leaf = Leaf

getCurrentNode :: MessageTree -> LogMessage
getCurrentNode (Node _ currentNode _) = currentNode

insert :: LogMessage -> MessageTree -> MessageTree
insert newNode tree =
  case newNode of
    Unknown _ -> tree
    LogMessage _ newTimeStamp _ ->
      let currentValue :: Int = getTimestamp (getCurrentNode tree)
       in case tree of -- If the tree is just a leaf node
            Leaf -> Node Leaf newNode Leaf
            Node leftNode currentMessage rightNode ->
              -- If the tree is not just a node
              if newTimeStamp < getTimestamp (getCurrentNode tree) -- If the new node's ts is < the tree's headNode
                then case leftNode of -- needs to go to the left
                  Leaf -> Node (Node Leaf newNode Leaf) (getCurrentNode tree) rightNode -- if the left node is a leaf
                  Node leftTree message rightTree -> Node (insert newNode leftNode) (getCurrentNode tree) rightNode -- if it's not a leaf
                else case rightNode of -- If the node's ts is greater than or equal to the tree's head node
                  Leaf -> Node leftNode (getCurrentNode tree) (Node Leaf newNode Leaf) -- If the right node is a leaf
                  Node leftTree message rightTree -> Node rightNode (getCurrentNode tree) (insert newNode rightNode) -- If the right node is not a leaf

-- Create a Build with a starting tree

buildRec :: [LogMessage] -> MessageTree -> MessageTree
buildRec messages tree =
  case messages of
    [] -> tree
    firstMessage : remainingMessages -> buildRec remainingMessages (insert firstMessage tree)

build :: [LogMessage] -> MessageTree
build [] = Leaf
build messages = buildRec messages Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder tree =
  case tree of
    Leaf -> []
    Node left message right -> inOrder left ++ message : inOrder right

isReleventError :: LogMessage -> Bool
isReleventError message =
  case message of
    LogMessage (Error level) _ msg -> level > 50
    _ -> False

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ msg) = msg

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages = map getMessage (filter isReleventError (inOrder (build messages)))

-- To Build -> ghc -main-is LogAnalysis -o main LogAnalysis.hs

main :: IO ()
main = do
  let sampleLogs =
        "E 47 1034 'What a pity it wouldn't stay!' sighed the Lory, as soon as it was quite \n\
        \I 3284 uhcd 00:03.3: LNVS) @ 0000000:0174715:00000000000] BOOTMEM\n\
        \I 4018 VGA mem Dynabled, nor memodiregis nosaved)\n\
        \I 3304 DMA32: 5\n\
        \I 2920 all of them bowed low.\n\
        \W 1654 'I kept all my limbs very supple\n\
        \I 2803 'They were learning to draw,' the Dormouse went on, yawning and rubbing\n\
        \I 2788 from?'\n\
        \I 2378 vanishing so suddenly: you make one quite giddy.'\n\
        \I 2669 pci_hci 000: 000:1c.1: RCONF(NETDEV_UP): 6232 has numalithis pregista_sone bus 00000-0x000009ff]\n\
        \I 861 #66WW-1a.2:e680fed13 00:1a00: adevicesetlindow [0xa0] (idged\n\
        \I 4751 pci 0001: [mem 000.58 (to 6-0xf43devialligurcpice wing ling IOM by 32 kipts ask: fa0000:1c.1: T4062\n\
        \I 4391 'Give your evidence,' the King repeated angrily, 'or I'll have you\n\
        \I 1883 cpi 0xf42 by EHCI ting CRT]\n\
        \I 4436 raw0::throl: atardb0fead: brit 10:24: Slock\n\
        \I 2382 Seto 00: Regispor suppow) st sp stus cold a00009dc0000003.2:e6:02. TCONF(NETDEV_UP): e3/0xa000000fed\n\
        \I 2751 Allencortenablerveres irq 9 pi 0000000000:1d.7: se fory 64\n\
        \I 2965 6 fortc009d908:c000000000\n\
        \I 1108 had vanished completely.\n\
        \I 1862 'Well! WHAT are you?' said the Pigeon. 'I can see you're trying to\n\
        \I 4980 would, in the after-time, be herself a grown woman; and how she would\n\
        \I 3260 pci_hci 00:0: Rounter to con upitchash 000 probe mem 0, PRT]\n\
        \I 3062 Brot D0 -> Gracputer 1)\n\
        \I 3720 ACPIC -> Inith to lassound [LNXVID: PCI E-ID - 000000: EC: D3hosave winged 000000000: Ressio+mem 0000000971c.0:1fff000d7c2: 000:00e winge memodevicesentel: rom caterve ouppoot-cardev/deve mort D0 Duot PM: Usign=3)\n\
        \I 2521 ACPI: wlap://indow [iore:1d.\n\
        \E 3 162 long passage, and the White Rabbit was still in sight, hurrying down it."
      logs = parse sampleLogs
  print (map getMessage (filter isReleventError (inOrder (build logs))))
