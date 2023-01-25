data RPS = Rock | Paper | Scissors

instance Show RPS where
    show Rock = "Rock"
    show Paper = "Paper"
    show Scissors = "Scissors"

whatBeats :: RPS -> RPS
whatBeats Rock = Paper
whatBeats Paper = Scissors
whatBeats Scissors = Rock