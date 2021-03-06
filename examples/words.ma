machine Automaton {
  transition A = "a"
  transition B = "b"
  transition C = "c"

  entry E {
    A -> S2,
    B -> E,
    C -> E,
  }

  state S2 {
    A -> S2,
    B -> S3,
    C -> E,
  }

  state S3 {
    A -> S2,
    B -> E,
    C -> S4,
  }

  accepting S4 {
    A -> S5,
    B -> S4,
    C -> S4,
  }


  accepting S5 {
    A -> S5,
    B -> S6,
    C -> S4,
  }

  accepting S6 {
    A -> S5,
    B -> S4,
    C -> E,
  }
}
