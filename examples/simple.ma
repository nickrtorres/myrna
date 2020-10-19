machine AyBee {
  transition A = "a"
  transition B = "b"

  entry E {
    A -> S2,
    B -> E,
  }

  terminal S2 {
    A -> E,
    B -> E,
  }
}

main {
  AyBee accepts "ba"
}
