 (1 (2 (3 4)))       ((2 (3 4)))
[*]---------------> [*]
 |                   |
 |                   |
 v                   v (2 (3 4))         ((3 4))
 1                  [*]---------------> [*]
                     |                   |
                     |                   |
                     v                   v (3 4)             (4)
                     2                  [*]---------------> [*]---------------> '()
                                         |                   |
                                         |                   |
                                         v                   v
                                         3                   4