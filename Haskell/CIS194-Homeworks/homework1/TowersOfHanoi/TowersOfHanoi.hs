module TowersOfHanoi where

-- A move consist of a tuple where the the left peg is where the disk
-- is being moved from and the right is where the disk is being moved to.
type Disks = Int
type Peg = Char
type Move = (Peg, Peg)

hanoi :: Disks -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a

