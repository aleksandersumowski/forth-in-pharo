Class {
	#name : #Plus,
	#superclass : #Operation,
	#category : #'Exercise@Forth'
}

{ #category : #'as yet unclassified' }
Plus >> performWithStack: stack [
	self assertStackHasTwoElements: stack.
   ^ (stack allButLast: 2) addLast: (stack atLast: 1 ) + (stack atLast: 2); yourself
]
