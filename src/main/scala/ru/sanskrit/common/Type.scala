package ru.sanskrit.common

enum Type:
  case Int
  case Func(a: Type, b: Type)
  case Sigma(aVal: Var, aType: Type, bType: Type)
