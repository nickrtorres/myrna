machine Automaton {
  transition A = 'a'
  transition B = 'b'
  transition C = 'c'

  entry E {
    A -> 2,
    B -> E,
    C -> E,
  }

  nonterminal 2 {
    A -> 2,
    B -> 3,
    C -> E,
  }

  nonterminal 3 {
    A -> 2,
    B -> E,
    C -> 4,
  }

  terminal 4 {
    A -> 5,
    B -> 4,
    C -> 4,
  }


  terminal 5 {
    A -> 5,
    B -> 6,
    C -> 4,
  }

  terminal 6 {
    A -> 5,
    B -> 4,
    C -> E,
  }
}

main {
  Automaton accepts "abc"
  Automaton rejects "zzz"
}
