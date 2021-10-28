Class {
	#name : #Forth,
	#superclass : #Object,
	#instVars : [
		'ops'
	],
	#classInstVars : [
		'ops'
	],
	#category : #'Exercise@Forth'
}

{ #category : #asserting }
Forth >> assert: stack hasAtLeast: minElements [
			stack isEmpty ifTrue: [ stack error: 'empty stack'].
			(stack size = 1) & (minElements > 1) ifTrue: [ stack error: 'only one value on the stack'].
]

{ #category : #'as yet unclassified' }
Forth >> evaluate: ops on: tokens withStack: stack [
 tokens isEmpty
	ifTrue: [  ^ stack ]
	ifFalse: [ |op  token|  
		token := tokens first.
		(token allSatisfy: [ :char| char isDigit ]) ifTrue: [ ^ self evaluate: ops on: (tokens allButFirst) withStack: (stack addLast: (tokens first asNumber);yourself) ].
		op := ops at: token asLowercase ifAbsent: [Object error: 'undefined operation'].
		 ^ op value: stack value: tokens ]
 
]

{ #category : #'as yet unclassified' }
Forth >> evaluateInstructions: aCollection [ 
	|tokens result ops customTokens|
 ops := Dictionary newFrom: {'+' -> [:s :t| self assert: s hasAtLeast: 2. 
                          self evaluate: ops on: (t allButFirst) withStack: ((s allButLast: 2)
	                                                                  addLast: (s atLast: 1 ) + (s atLast: 2); yourself)].
									 '-' -> [:s :t|   self assert: s hasAtLeast: 2.
	                                          self evaluate: ops on:  (t allButFirst) withStack: ((s allButLast: 2)
	                                                                  addLast: (s atLast: 2 ) - (s atLast: 1); yourself)
																						 ] .
									 '*' -> [:s :t|  self assert: s hasAtLeast: 2.
	                                         self evaluate: ops on:  (t allButFirst) withStack: ((s allButLast: 2)
	                                                                  addLast: (s atLast: 1 ) * (s atLast: 2); yourself)
																						 ].
									 '/' -> [:s :t|   self assert: s hasAtLeast: 2.
										         (s atLast: 1) = 0 ifTrue: [ Object error: 'divide by zero' ].
	                                         self evaluate: ops on:  (t allButFirst) withStack: ((s allButLast: 2)
	                                                                  addLast: ((s atLast: 2) / (s atLast: 1)) asInteger; yourself)
																						 ].
									'dup' -> [:s :t| self assert: s hasAtLeast: 1. self evaluate: ops on:  (t allButFirst) withStack: (s addLast: s last; yourself) ].
									'drop' -> [:s :t| self assert: s hasAtLeast: 1. self evaluate: ops on:  (t allButFirst) withStack: s allButLast ].
									'over' -> [:s :t| self assert: s hasAtLeast: 2. self evaluate: ops on:  (t allButFirst) withStack: (s addLast: (s atLast: 2);yourself ) ].
									'swap' -> [ :s :t|  self assert: s hasAtLeast: 2. self evaluate: ops on:  (t allButFirst) withStack:  (s swap: (s size) with: (s size - 1) )].
									}.
	customTokens := Dictionary new.
	(aCollection select: [ :each| (each beginsWith: ':') and: (each endsWith: ';')  ]) do: [ :custom|
		|op tokenValue|
		op := (custom asLowercase splitOn: ' ') allButFirst allButLast.
		(op first allSatisfy: [ :char| char isDigit ]) ifTrue: [ Object error: 'illegal operation' ].
		tokenValue := op allButFirst collect: [ :token | customTokens at: token ifAbsent: token asLowercase  ].
		customTokens at: op first put: tokenValue flattened 
		 ].
	tokens  := (((aCollection reject: [ :each| (each beginsWith: ':') and: (each endsWith: ';')  ]) first splitOn: ' ') collect: [:token | customTokens at: token asLowercase ifAbsent: token]) flattened.
	result := self evaluate: ops on: tokens withStack: OrderedCollection new.
	^ (result collect: [:el| el asNumber]) asArray.
]

{ #category : #initialization }
Forth >> initialize [ 
 ops := Dictionary newFrom: {'+' -> [:s :t| self assert: s hasAtLeast: 2. 
                          self evaluate: ops on: (t allButFirst) withStack: ((s allButLast: 2)
	                                                                  addLast: (s atLast: 1 ) + (s atLast: 2); yourself)].
									 '-' -> [:s :t|   self assert: s hasAtLeast: 2.
	                                          self evaluate: ops on:  (t allButFirst) withStack: ((s allButLast: 2)
	                                                                  addLast: (s atLast: 2 ) - (s atLast: 1); yourself)
																						 ] .
									 '*' -> [:s :t|  self assert: s hasAtLeast: 2.
	                                         self evaluate: ops on:  (t allButFirst) withStack: ((s allButLast: 2)
	                                                                  addLast: (s atLast: 1 ) * (s atLast: 2); yourself)
																						 ].
									 '/' -> [:s :t|   self assert: s hasAtLeast: 2.
										         (s atLast: 1) = 0 ifTrue: [ Object error: 'divide by zero' ].
	                                         self evaluate: ops on:  (t allButFirst) withStack: ((s allButLast: 2)
	                                                                  addLast: ((s atLast: 2) / (s atLast: 1)) asInteger; yourself)
																						 ].
									'dup' -> [:s :t| self assert: s hasAtLeast: 1. self evaluate: ops on:  (t allButFirst) withStack: (s addLast: s last; yourself) ].
									'drop' -> [:s :t| self assert: s hasAtLeast: 1. self evaluate: ops on:  (t allButFirst) withStack: s allButLast ].
									'over' -> [:s :t| self assert: s hasAtLeast: 2. self evaluate: ops on:  (t allButFirst) withStack: (s addLast: (s atLast: 2);yourself ) ].
									'swap' -> [ :s :t|  self assert: s hasAtLeast: 2. self evaluate: ops on:  (t allButFirst) withStack:  (s swap: (s size) with: (s size - 1) )].
									}.
]
