# Object-Oriented PASTA

This project is extending the language “PASTA” (which was deveoped in c) to be an object-oriented
language. It adds additional language features, which enable users to define classes, methods as
well as create instances. While developing this project, I have used instructions of a suggested
assignment from the book “Structure and Interpretation of Computer Programs”, which can be
found in here: https://mitpress.mit.edu/sicp/psets/ps7oop/readme.html
Please refer to file 'README.pdf' for more detailed instructions.

## Define Class
  A ‘define-class’ special expression takes in the name of the class, a list of superclass
which the class inherits from, and zero or more names for slots (the instance variables). Every
class has to have at least one super class. There is a predefined class ‘object’, and any other class
is its subclass. Classes are stored in the top level of the environment. As a result of that, name of
each class has to be unique. A class can have multiple superclasses. Instances belong to a class
can be applied methods that belong to that class’s superclasses. When defining a new class, its
superclass’s slots will also be carried over.

## Define Method
  A ‘define-method’ special expression takes in the name of the method, name of the class
that this method can be applied to, and an lambda-exp. Names of methods are stored the same as
names of classes, so their have to be unique, and classes and methods could not share the same
name either. You could not define a method that could be applied to an non-existing class. The
lambda-exp can take in extra arguments, just like our normal lambda expression. Note that
lambda expression has to have ‘self’ as the first parameter.

## Create Instance
  Users can create instances by the ‘make’ expression. The expression takes name of a
class, and then slots’ names and values pairs. 

## Apply method
When applying a method to an instance, we need to use the ‘tell’ expression. It takes the
instance’s name, name of the method, and zero or more extra arguments. 

## Set Expression
Set expression needs to take three arguments. It takes the name of the instance, name of
the slot, and the new value you want to assign to the slot. This expression will always return the
updated instance value.

## ToDos and Current Problems
  Since methods and classes are all stored together in the top level. Methods could not have
same names, even if they are for different classes. I think it will be more intuitive if the
interpreter could handle methods with the same name but belong to different class. In addition to
that, in the current version, subclasses can not override methods from their superclasses. It will
be great if the language have both ‘override’ and ‘overload’(methods that have same name, but
take in different parameters) functions. 
