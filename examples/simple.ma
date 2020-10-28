machine AyBee {
  transition A = "a"
  transition B = "b"

  entry E {
    A -> S1,
    B -> E,
  }

  state S1 {
    A -> S2,
    B -> S2,
  }

  accepting S2 {
    A -> E,
    B -> E,
  }
}
