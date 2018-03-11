#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
execname := '$RCSfile: cmdline.st,v $' =~ '.*: (.*),v.*' at: 1.

"catfile.st"
FileStream extend [
   nl := Character nl.
   catFile [
      |count textline|
      count := 0.
      [self atEnd not] whileTrue: [
         count := count + 1.
         textline :=self nextLine.
         count asString size to: 5 do: [:skip| stdout << ' '].
         stdout << count << ': ' << textline << nl.
      ]
   ]
]

"charfreq.st"
nl := Character nl.

Character extend [
   isPrint [
      ^ (Character space < self) & (self <= $~)
   ]
   visible [
      self isPrint ifTrue: [^  ' ' , self asString , ' ']
                   ifFalse: [ self asInteger = 256 ifTrue: [^ 'EOF']
                              ifFalse: [^ 'x' , 
                                  ((self asInteger printStringRadix: 16) 
                                      copyFrom: 4)]    
                            ]
   ]
]

Object subclass: ZeroArray [
   |theArray|

   ZeroArray class >> new [
      self shouldNotImplement.
   ]

   ZeroArray class >> new: size [
      |result|
      result := super new.
      result init: size.
      ^ result
   ]

   init: size [
      theArray := Array new: size.
   ]

   size [
      ^ theArray size.
   ]

   at: index [
      ^ theArray at: index + 1.
   ]

   at: index put: value [
      ^ theArray at: index + 1 put: value.
   ]

   incr: index [
      (self at: index) isNil
         ifTrue: [ self at: index put: 0].
      self at: index put: (self at: index) + 1.
   ]

   keysAndValuesDo: aBlock [
      (0 to: (self size) - 1) do: [:index |
         aBlock value: index value: (self at: index).
      ]
   ]
]

"tree.st"
freqtable := ZeroArray new: 257.
freqtable at:256 put:1.

Object subclass: Leaf [
   |char count isLeaf|
   char [ ^ char ]
   count [ ^ count ]
   isLeaf [^ isLeaf]
   
   Leaf class >> new [
      self shouldNotImplement
   ]

   Leaf class >> new: aChar count: aCount [
      |result|
      result := super new.
      result setChar: aChar andCount: aCount.      
      ^result
   ]

   setChar: aChar andCount: aCount [
      char := aChar.
      count := aCount.
      isLeaf:=true.
   ]

   <= other [
      ^ (count < other count)
      | ((count = other count) & (char <= other char))
   ]

   printBase: aStream [
      ^ aStream << self class << '(' << char << ',' << count
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   inorder: visitor prefix: string [
      visitor value: char value: string.
   ]
   
   postorder: visitor [
      visitor  value:char value: isLeaf.
   ]
]

Leaf subclass: Tree [
   |left right|

   Tree class >> new: aChar count: aCount [
      self shouldNotImplement
   ]

   Tree class >> new: aChar count: aCount left: aLeft right: aRight [
      |result|
      result := super new: aChar count: aCount.
      result setLeft: aLeft andRight: aRight.
      ^ result
   ]

   setLeft: aLeft andRight: aRight [
      left := aLeft.
      right := aRight.
      isLeaf:=false.
   ]

   left  [ ^ left]
   right [ ^ right]

   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   inorder: visitor prefix: string [
      left inorder: visitor prefix: string, '0'.
      right inorder: visitor prefix: string, '1'.
   ]
   
   postorder: visitor[      
      left postorder: visitor .
      right postorder: visitor .  
      visitor value: char value:isLeaf.
   ]
]

"outbits.st"
Object subclass: OutBits [
   |bitIndex currentByte myStream|
   OutBits class >> new [
      self shouldNotImplement.
   ]
   OutBits class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   clearByte [
      bitIndex := 8.
      currentByte := 0.
   ]
   init: fileStream [
      myStream := fileStream.
      self clearByte.
   ]
   flushByte [
         myStream nextPutByte: currentByte.
         self clearByte.

   ]
   writeBit: bit [
      currentByte := currentByte bitAt: bitIndex put: bit.
      bitIndex := bitIndex - 1.
      bitIndex = 0 ifTrue: [
        self flushByte].
   ]
]

"bitstream.st"
Object subclass: BitStream [
   |bitIndex byte myStream|
   BitStream class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   init: fileStream [
      myStream := fileStream.
      bitIndex := 1.
   ]
   nextBit [
      bitIndex = 1 ifTrue: [
         byte := myStream next.
         bitIndex := 9.
      ].
      bitIndex := bitIndex - 1.
      ^byte value bitAt: bitIndex
   ]
   atEnd [
      ^bitIndex = 1 and: [myStream atEnd]
   ]
]

"options"
optD := false.
optT := false.
optC := false.
optU := false.

queue := SortedCollection new.

|inputfile readfile outputfile dcpfile outfilename argc|
argc:=0.

Smalltalk arguments: '-d -t -c -u'
do: [:opt :arg |

   (opt = $d) ifTrue: [ optD := true. ].
   (opt = $t) ifTrue: [ optT := true. ].
   (opt = $c) ifTrue: [ optC := true. ].
   (opt = $u) ifTrue: [ optU := true. ].

  (arg ~= nil) ifTrue: [
    
     argc = 0 
        ifTrue:[
          optD ifTrue:[
          stdout <<'input filename: ' << arg <<nl.].

          inputfile := FileStream open: arg
                    mode: FileStream read.
          readfile := FileStream open: arg
                    mode: FileStream read.
         ]

        ifFalse:[
           optD ifTrue:[
           stdout <<'output filename: ' << arg <<nl.].
            outputfile := FileStream open: arg
                       mode: FileStream write.].
  
     argc := argc + 1.
   ].
].

|tree ExitStatus|
ExitStatus:=0.

optD ifTrue: [
stdout<<nl<<'Exit Status: '<<ExitStatus<<nl.
].

optU ifFalse:[
    encodeTable := ZeroArray new: 257.

[inputfile atEnd not] whileTrue: [
   |refChar|
   refChar := inputfile next asInteger.
   freqtable incr: refChar.
].

freqtable keysAndValuesDo: [:key :count |
   (count notNil and: [count > 0]) ifTrue: [
      queue add: (Leaf new: key count: count).
   ]
].

[queue size >1 ] whileTrue: [
   |leftchild rightchild|
   
   leftchild := queue removeFirst.
   rightchild := queue removeFirst.
   newtree := ( Tree new: (leftchild char)
                      count: (leftchild count) +  (rightchild count)
                      left: leftchild right: rightchild).
   queue add: newtree.
].
tree:=queue removeFirst.


tree inorder: [:char :string |
   encodeTable at: char put: string.
] prefix: ''.

binarytree:=''.
tree postorder: [:char :isLeaf |

   isLeaf  ifFalse:[binarytree:=binarytree,'1'.]
                ifTrue:[
                   bin:=''.
                   char = 256
                        ifTrue:[bin:='000000001']
                        ifFalse:[
                           char=0 ifTrue:[bin:='000000000']
                                  ifFalse:[
                                     8 to:1 by: -1 do:[:i|
                                     bin:=bin,((char bitAt:i)
                                                       printString).
                                   ]
                        ].               
                 ].
                 binarytree:=(binarytree,'0',bin).
              ].
].

optT ifTrue:[
     stdout << 'Encode table' << nl.
     encodeTable keysAndValuesDo: [:key :count |
     (count notNil and: [count > 0]) ifTrue: [
        stdout <<  key asCharacter visible << '     '
               << (freqtable at: key) <<'    '<<count
               << nl.
     ].
    ].
].

optC  ifTrue:[ 

outputfile~=nil ifTrue:[
   
    optD ifTrue: [
    stdout <<nl<< 'optC = '<<optC 
          <<'outputfile'<< nl.].
  
   outBytes := OutBits new: outputfile.    
   1 to: (binarytree size) by: 1 do: [:index |
         bit:= ((binarytree at: index) asInteger) - 48.
         outBytes writeBit: bit.
         ].

   outBytes writeBit: 1.
   [readfile atEnd not] whileTrue: [
      |ordChar|
       ordChar := readfile next asInteger.
       code:= (encodeTable at: ordChar). 
       1 to: (code size) by: 1 do: [:index |
         bit:= ((code at: index) asInteger) - 48.
         outBytes writeBit: bit.
          ].
       ].

   eof:= (encodeTable at: 256).
   1 to: (eof size) by: 1 do: [:index |
         bit:= ((eof at: index) asInteger) - 48.
         outBytes writeBit: bit.].     
   outBytes flushByte.
]
ifFalse:[
   optD ifTrue:[
   stdout <<nl<< 'optC = '<<optC
              <<'no outputfile'<< nl.].
    
   stdout <<nl<< 'Encoded input to binary' << nl.
   stdout <<binarytree<<1.      
    
     [readfile atEnd not] whileTrue: [
           |ordChar|
           ordChar := readfile next asInteger.
           code:= (encodeTable at: ordChar).
           stdout <<code. 
        ].
      eof:= (encodeTable at: 256).
      stdout<<eof.
   ].    
 ].
].

(optU) ifTrue:[
   |iseof outBytes stack|
   iseof:=false.

   optD ifTrue: [
   stdout<<'Decompression'<<nl.].

   outBytes := OutBits new: outputfile.  
   stack := OrderedCollection new.
   fileBits := BitStream new: inputfile.
   treebuilt:=false.
   reconstructedTree:=''.
   node:=''.   
   
   [(fileBits atEnd not) & (iseof not)] whileTrue: [

     treenode:=fileBits nextBit.
     leafchar:=0.

     treebuilt ifFalse:[

        treenode=0 ifTrue:[

           8 to:1 by:-1 do:[:i|
              bit:=fileBits nextBit.
              
            leafchar:=leafchar bitAt:i put:bit.
           ].

           leafchar=0 ifTrue:[
             specialbit:=fileBits nextBit.
             specialbit=1 ifTrue:[
              leafchar:=leafchar bitAt:9 put:1.
             ].
           ].
         
          stack add: (Leaf new: leafchar count: '').

        ].

        treenode=1 ifTrue:[
                 
           stack size>1 ifTrue:[
           rightchild:=stack removeLast.
           leftchild:=stack removeLast.
           
           newtree := (Tree new: -1
                      count: ''
                      left: leftchild right: rightchild).
             stack add: newtree.
           ] ifFalse:[
             treebuilt:=true.
             
             reconstructedTree:=stack removeLast.
             node:=reconstructedTree.
           ].
        ].
     ]
     ifTrue:[
       node isLeaf ifTrue:[
                ((node char)=256) ifTrue:[
                    iseof:=true.
                    ] ifFalse:[
                     stdout nextPut: (Character value: (node char)).
                  node:=reconstructedTree.].
               ]ifFalse:[
        treenode=0 ifTrue:[
                
                node:=node left.

                node isLeaf ifTrue:[
                iseof not ifTrue:[  
                    ((node char)=256) ifTrue:[
                    iseof:=true.


                    ] ifFalse:[

                     outputfile=nil ifTrue:[

                     stdout nextPut: (Character value: (node char)).
                     ]
                     ifFalse:[
                        8 to: 1 by: -1 do: [:index |
                           bit:= (node char) bitAt: index.
                           outBytes writeBit: bit.
                        ].
                     ].
                     node:=reconstructedTree.

                    ].
                ].
               ]
           ] ifFalse:[
               
               node:=node right.

               node isLeaf ifTrue:[
                iseof not ifTrue:[
                    ((node char)=256) ifTrue:[
                    iseof:=true.

                    ] ifFalse:[

                     outputfile=nil ifTrue:[

                     stdout nextPut: (Character value: (node char)).
                            ]
                     ifFalse:[
                        8 to: 1 by: -1 do: [:index |
                           bit:= (node char) bitAt: index.
                           outBytes writeBit: bit.
                        ].
                     ].
                    node:=reconstructedTree.

                    ].
                ].
               ]
           ].
       ].
     ].
   ].
].  


inputfile close.
outputfile~=nil ifTrue:[
outputfile close.
].

