package ru.sanskrit.common

enum Type:
  case Int
  case IntArray(n: Int)
  case Func(a: Type, b: Type)
  case Sigma(aVar: Var, aType: Type, bType: Type)
