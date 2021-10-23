Class {
	#name : #Operation,
	#superclass : #Object,
	#instVars : [
		'minStackSize'
	],
	#category : #'Exercise@Forth'
}

{ #category : #asserting }
Operation >> assertStackHasElement: stack [
			stack isEmpty ifTrue: [ stack error: 'empty stack'].
]

{ #category : #asserting }
Operation >> assertStackHasTwoElements: stack [
			stack isEmpty ifTrue: [ stack error: 'empty stack'].
			stack size = 1 ifTrue: [ stack error: 'only one value on the stack'].
]
