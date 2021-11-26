--
-- EPITECH PROJECT, 2021
-- HAL
-- File description:
-- Interpreter ParserSpec
--

module Interpreter.ParserSpec   ( spec ) where

import Test.Hspec               ( Spec
                                , it
                                )

import Interpreter.Parser       ( buildExpressionsTrees
                                , Tree(..)
                                , ProcedureArg(..)
                                )

import Interpreter.Lexer        ( tokenize )

newtype TestTree = TestTree Tree
instance Eq TestTree where
   (TestTree (Node left_trees))                 == (TestTree (Node right_trees))                    = map TestTree left_trees   == map TestTree right_trees
   (TestTree (Leaf (Number left_value)))        == (TestTree (Leaf (Number right_value)))           = left_value                == right_value
   (TestTree (Leaf (Symbol left_symbol)))       == (TestTree (Leaf (Symbol right_symbol)))          = left_symbol               == right_symbol
   (TestTree (Leaf (UncreatedList left_trees))) == (TestTree (Leaf (UncreatedList right_trees)))    = map TestTree left_trees   == map TestTree right_trees
   _                                            == _                                                = False

spec :: Spec
spec = do
    it "Correct - cons with values" $ do
        map TestTree (buildExpressionsTrees (tokenize "(cons 1 2)"))
            == map TestTree [
              Node [
                Leaf (Symbol "cons"), Leaf (Number 1.0), Leaf (Number 2.0)
              ]
            ]
    it "Correct - doubly nested cons" $ do
        map TestTree (buildExpressionsTrees (tokenize "(cons (cons 1 2) (cons 3 4))"))
            == map TestTree [
              Node [
                Leaf (Symbol "cons"),
                  Node [
                    Leaf (Symbol "cons"), Leaf (Number 1.0),Leaf (Number 2.0)
                  ],
                Node [
                  Leaf (Symbol "cons"), Leaf (Number 3.0), Leaf (Number 4.0)
                ]
              ]
            ]
    it "Correct - doubly nested cons with nil" $ do
        map TestTree (buildExpressionsTrees (tokenize "(cons (cons 1 '()) (cons 3 4))"))
            == map TestTree [
              Node [
                Leaf (Symbol "cons"),
                  Node [
                    Leaf (Symbol "cons"), Leaf (Number 1.0), Leaf (UncreatedList [])
                  ],
                  Node [
                    Leaf (Symbol "cons"), Leaf (Number 3.0), Leaf (Number 4.0)
                  ]
              ]
            ]
    it "Correct - foo define" $ do
        map TestTree (buildExpressionsTrees (tokenize "((define foo '21))"))
            == map TestTree [
              Node [
                Node [
                  Leaf (Symbol "define"),
                    Leaf (Symbol "foo"),
                    Node [
                      Leaf (Symbol "quote"), Leaf (Number 21.0)
                    ]
                ]
              ]
            ]
    it "Correct - foo cond" $ do
        map TestTree (buildExpressionsTrees (tokenize "(cond (#f 'foo) (#f foo) (#t 42) (#t '42))"))
            == map TestTree [
              Node [
                Leaf (Symbol "cond"),
                Node [
                  Leaf (Symbol "#f"),
                  Node [
                    Leaf (Symbol "quote"), Leaf (Symbol "foo")
                  ]
                ],
                Node [
                  Leaf (Symbol "#f"),
                  Leaf (Symbol "foo")
                ],
                Node [
                  Leaf (Symbol "#t"),
                  Leaf (Number 42.0)
                ],
                Node [
                  Leaf (Symbol "#t"),
                  Node [
                    Leaf (Symbol "quote"),
                    Leaf (Number 42.0)
                  ]
                ]
              ]
            ]
    it "Correct - factorial define" $ do
        map TestTree (buildExpressionsTrees (tokenize "(define (fact x) (cond ((eq? x 1) 1) (#t (* x (fact (- x 1))))))"))
            == map TestTree [
              Node [
                Leaf (Symbol "define"),
                  Node [
                    Leaf (Symbol "fact"), Leaf (Symbol "x")
                  ],
                Node [
                  Leaf (Symbol "cond"),
                  Node [
                    Node [
                      Leaf (Symbol "eq?"),
                        Leaf (Symbol "x"),
                        Leaf (Number 1.0)
                    ],
                    Leaf (Number 1.0)
                  ],
                  Node [
                    Leaf (Symbol "#t"),
                    Node [
                      Leaf (Symbol "*"),
                      Leaf (Symbol "x"),
                      Node [
                        Leaf (Symbol "fact"),
                        Node [
                          Leaf (Symbol "-"),
                          Leaf (Symbol "x"),
                          Leaf (Number 1.0)
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]
    it "Correct - merge sort" $ do
        map TestTree (buildExpressionsTrees (tokenize (
            "(define (null? l) (eq? l '()))" ++
            "(define (merge-lists l1 l2)" ++
            "   (cond ((null? l1) l2)" ++
            "       ((null? l2) l1)" ++
            "       ((< (car l1) (car l2)) (cons (car l1) (merge-lists (cdr l1) l2)))" ++
            "       (#t                    (cons (car l2) (merge-lists l1 (cdr l2))))))" ++
            "" ++
            "(define (split-half l l1 l2)" ++
            "   (cond ((null? l) (cons l1 l2))" ++
            "       ((null? (cdr l)) (split-half (cdr l) (cons (car l) l1) l2))" ++
            "       (#t (split-half (cdr (cdr l))" ++
            "                       (cons (car l) l1)" ++
            "                       (cons (car (cdr l)) l2)))))" ++
            "" ++
            "(define (merge-sort lst)" ++
            "   (cond ((null? lst) '())" ++
            "       ((null? (cdr lst)) lst)" ++
            "       (#t (let ((lsts (split-half lst '() '())))" ++
            "               (merge-lists (merge-sort (car lsts))" ++
            "                           (merge-sort (cdr lsts)))))))")))
            == map TestTree [
                Node [
                  Leaf (Symbol "define"),
                    Node [
                      Leaf (Symbol "null?"),
                      Leaf (Symbol "l")
                    ],
                    Node [
                      Leaf (Symbol "eq?"),
                      Leaf (Symbol "l"),
                      Leaf (UncreatedList [])
                    ]
                ],
                Node [
                  Leaf (Symbol "define"),
                    Node [
                      Leaf (Symbol "merge-lists"),
                      Leaf (Symbol "l1"),
                      Leaf (Symbol "l2")
                    ],
                  Node [
                    Leaf (Symbol "cond"),
                      Node [
                        Node [
                          Leaf (Symbol "null?"),
                          Leaf (Symbol "l1")
                        ],
                        Leaf (Symbol "l2")
                      ],
                      Node [
                        Node [
                          Leaf (Symbol "null?"),
                          Leaf (Symbol "l2")
                        ],
                        Leaf (Symbol "l1")
                      ],
                      Node [
                        Node [
                          Leaf (Symbol "<"),
                          Node [
                            Leaf (Symbol "car"),
                            Leaf (Symbol "l1")
                          ],
                        Node [
                          Leaf (Symbol "car"),
                          Leaf (Symbol "l2")
                        ]
                      ],
                      Node [
                        Leaf (Symbol "cons"),
                        Node [
                          Leaf (Symbol "car"),
                          Leaf (Symbol "l1")
                        ],
                        Node [
                          Leaf (Symbol "merge-lists"),
                          Node [
                            Leaf (Symbol "cdr"),
                            Leaf (Symbol "l1")
                          ],
                          Leaf (Symbol "l2")
                        ]
                      ]
                    ],
                    Node [
                      Leaf (Symbol "#t"),
                      Node [
                        Leaf (Symbol "cons"),
                        Node [
                          Leaf (Symbol "car"),
                          Leaf (Symbol "l2")
                        ],
                        Node [
                          Leaf (Symbol "merge-lists"),
                          Leaf (Symbol "l1"),
                          Node [
                            Leaf (Symbol "cdr"),
                            Leaf (Symbol "l2")
                          ]
                        ]
                      ]
                    ]
                  ]
                ],
                Node [
                  Leaf (Symbol "define"),
                  Node [
                    Leaf (Symbol "split-half"),
                    Leaf (Symbol "l"),
                    Leaf (Symbol "l1"),
                    Leaf (Symbol "l2")
                  ],
                  Node [
                    Leaf (Symbol "cond"),
                    Node [
                      Node [
                        Leaf (Symbol "null?"),
                        Leaf (Symbol "l")
                      ],
                      Node [
                        Leaf (Symbol "cons"),
                        Leaf (Symbol "l1"),
                        Leaf (Symbol "l2")
                      ]
                    ],
                    Node [
                      Node [
                        Leaf (Symbol "null?"),
                        Node [
                          Leaf (Symbol "cdr"),
                          Leaf (Symbol "l")
                        ]
                      ],
                      Node [
                        Leaf (Symbol "split-half"),
                        Node [
                          Leaf (Symbol "cdr"),
                          Leaf (Symbol "l")
                        ],
                      Node [
                        Leaf (Symbol "cons"),
                        Node [
                          Leaf (Symbol "car"),
                          Leaf (Symbol "l")
                        ],
                        Leaf (Symbol "l1")
                      ],
                      Leaf (Symbol "l2")
                    ]
                  ],
                  Node [
                    Leaf (Symbol "#t"),
                    Node [
                      Leaf (Symbol "split-half"),
                      Node [
                        Leaf (Symbol "cdr"),
                        Node [
                          Leaf (Symbol "cdr"),
                          Leaf (Symbol "l")
                        ]
                      ],
                      Node [
                        Leaf (Symbol "cons"),
                        Node [
                          Leaf (Symbol "car"),
                          Leaf (Symbol "l")
                        ],
                        Leaf (Symbol "l1")
                      ],
                      Node [
                        Leaf (Symbol "cons"),
                        Node [
                          Leaf (Symbol "car"),
                          Node [
                            Leaf (Symbol "cdr"),
                            Leaf (Symbol "l")
                          ]
                        ],
                        Leaf (Symbol "l2")
                      ]
                    ]
                  ]
                ]
              ],
              Node [
                Leaf (Symbol "define"),
                Node [
                  Leaf (Symbol "merge-sort"),
                  Leaf (Symbol "lst")
                ],
                Node [
                  Leaf (Symbol "cond"),
                  Node [
                    Node [
                      Leaf (Symbol "null?"),
                      Leaf (Symbol "lst")
                    ],
                    Leaf (UncreatedList [])
                  ],
                  Node [
                    Node [
                      Leaf (Symbol "null?"),
                      Node [
                        Leaf (Symbol "cdr"),
                        Leaf (Symbol "lst")
                      ]
                    ],
                    Leaf (Symbol "lst")
                  ],
                  Node [
                    Leaf (Symbol "#t"),
                    Node [
                      Leaf (Symbol "let"),
                      Node [
                        Node [
                          Leaf (Symbol "lsts"),
                          Node [
                            Leaf (Symbol "split-half"),
                            Leaf (Symbol "lst"),
                            Leaf (UncreatedList []),
                            Leaf (UncreatedList [])
                          ]
                        ]
                      ],
                      Node [
                        Leaf (Symbol "merge-lists"),
                        Node [
                          Leaf (Symbol "merge-sort"),
                          Node [
                            Leaf (Symbol "car"),
                            Leaf (Symbol "lsts")
                          ]
                        ],
                        Node [
                          Leaf (Symbol "merge-sort"),
                          Node [
                            Leaf (Symbol "cdr"),
                            Leaf (Symbol "lsts")
                          ]
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]
