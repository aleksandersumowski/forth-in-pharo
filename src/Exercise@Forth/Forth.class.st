Class {
	#name : #Forth,
	#superclass : #Object,
	#classInstVars : [
		'ops'
	],
	#category : #'Exercise@Forth'
}

{ #category : #asserting }
Forth >> assertStackBigEnough: stack [
			stack isEmpty ifTrue: [ stack error: 'empty stack'].
			stack size = 1 ifTrue: [ stack error: 'only one value on the stack'].
]

{ #category : #'as yet unclassified' }
Forth >> evaluate: tokens withStack: stack [
|ops|
 ops := Dictionary newFrom: {'+' -> [:s :t| self assertStackBigEnough: s. 
                         ^ self evaluate: (t allButFirst) withStack: ((s allButLast: 2)
	                                                                  addLast: (s atLast: 1 ) + (s atLast: 2); yourself)].
									 '-' -> [:s :t|   self assertStackBigEnough: s. 
	                                         ^  self evaluate: (t allButFirst) withStack: ((s allButLast: 2)
	                                                                  addLast: (s atLast: 2 ) - (s atLast: 1); yourself)
																						 ] .
									 '*' -> [:s :t|  self assertStackBigEnough: s. 
	                                         ^  self evaluate: (t allButFirst) withStack: ((s allButLast: 2)
	                                                                  addLast: (s atLast: 1 ) * (s atLast: 2); yourself)
																						 ].
									 '/' -> [:s :t|   self assertStackBigEnough: s. 
										         (s atLast: 1) = 0 ifTrue: [ Object error: 'divide by zero' ].
	                                         ^  self evaluate: (t allButFirst) withStack: ((s allButLast: 2)
	                                                                  addLast: ((s atLast: 2) / (s atLast: 1)) asInteger; yourself)
																						 ].
									'dup' -> [:s :t|  ^ self evaluate: (t allButFirst) withStack: (s addLast: s last; yourself) ].
									'drop' -> [:s :t|  ^ self evaluate: (t allButFirst) withStack: s allButLast ].
									'over' -> [:s :t| ^ self evaluate: (t allButFirst) withStack: (s addLast: (s atLast: 2);yourself ) ].
									'swap' -> [ :s :t|   self assertStackBigEnough: s. ^ self evaluate: (t allButFirst) withStack:  (s swap: (s size) with: (s size - 1) )].
									}.
 tokens isEmpty
	ifTrue: [  ^ stack ]
	ifFalse: [ |op absentBlock|  
		absentBlock := [:s :t| ^ self evaluate: (t allButFirst) withStack: (s addLast: (t first asNumber);yourself)].
		op := ops at: tokens first asLowercase ifAbsent: [ absentBlock ] .
		 ^ op value: stack value: tokens ]
 
]

{ #category : #'as yet unclassified' }
Forth >> evaluateInstructions: aCollection [ 
	|tokens result|
	tokens := (aCollection first splitOn: ' ').
	result := self evaluate: tokens withStack: OrderedCollection new.
	^ (result collect: [:el| el asNumber]) asArray.
]
