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
 ops := Dictionary newFrom: {'+' -> [ self assertStackBigEnough: stack. 
                         ^ self evaluate: (tokens allButFirst) withStack: ((stack allButLast: 2)
	                                                                  addLast: (stack atLast: 1 ) + (stack atLast: 2); yourself)].
									 '-' -> [  self assertStackBigEnough: stack. 
	                                         ^  self evaluate: (tokens allButFirst) withStack: ((stack allButLast: 2)
	                                                                  addLast: (stack atLast: 2 ) - (stack atLast: 1); yourself)
																						 ] .
									 '*' -> [ self assertStackBigEnough: stack. 
	                                         ^  self evaluate: (tokens allButFirst) withStack: ((stack allButLast: 2)
	                                                                  addLast: (stack atLast: 1 ) * (stack atLast: 2); yourself)
																						 ].
									 '/' -> [  self assertStackBigEnough: stack. 
										         (stack atLast: 1) = 0 ifTrue: [ Object error: 'divide by zero' ].
	                                         ^  self evaluate: (tokens allButFirst) withStack: ((stack allButLast: 2)
	                                                                  addLast: ((stack atLast: 2) / (stack atLast: 1)) asInteger; yourself)
																						 ].
									'dup' -> [ ^ self evaluate: (tokens allButFirst) withStack: (stack addLast: stack last; yourself) ].
									'drop' -> [ ^ self evaluate: (tokens allButFirst) withStack: stack allButLast ].
									'over' -> [^ self evaluate: (tokens allButFirst) withStack: (stack addLast: (stack atLast: 2);yourself ) ].
									'swap' -> [   self assertStackBigEnough: stack. ^ self evaluate: (tokens allButFirst) withStack:  (stack swap: (stack size) with: (stack size - 1) )].
									}.
 tokens isEmpty
	ifTrue: [  ^ stack ]
	ifFalse: [ |op absentBlock|  
		absentBlock := [ ^ self evaluate: (tokens allButFirst) withStack: (stack addLast: (tokens first asNumber);yourself)].
		op := ops at: tokens first asLowercase ifAbsent: [ absentBlock ] .
		 ^ op value ]
 
]

{ #category : #'as yet unclassified' }
Forth >> evaluateInstructions: aCollection [ 
	|tokens result|
	tokens := (aCollection first splitOn: ' ').
	result := self evaluate: tokens withStack: OrderedCollection new.
	^ (result collect: [:el| el asNumber]) asArray.
]
