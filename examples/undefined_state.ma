machine AyBee {
  transition A = "a"
  transition B = "b"

  entry E {
    A -> S1,
    C -> E,
  }
}
