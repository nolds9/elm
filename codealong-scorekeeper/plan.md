# Planning Que

## Model

Model is a record made up of:

```elm
Model =
  { students : List Student
  , newStudentName : String
  , currentStudent : Student
  }
```

Student type  shape

```elm
{ id : Int
, name : String
}
```

Todo InitialModel:

```elm
  initModel =
    Model [] ""
```

```elm

```

## Update

Actions :
- Add a new student to the queue
- Mark a student as current
- Remove a student from the queue
- Cancel Player Input

Bonus:
- Update Position of a student

Todo: create Message Union types

Todo: create update functions

## View

Components:
- container div
  - header
  - list of students
    - button to delete student from queue
    - button to make student Current
  - currentStudent display
  - new Student form

Todo: create view functions for each of the above 
