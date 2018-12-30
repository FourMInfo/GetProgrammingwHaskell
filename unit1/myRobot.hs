robot (name,attack,hp) message = message (name,attack,hp)

name (n,_,_) = n
attack (_,a,_) = a
hp (_,_,hp) = hp

getName aRobot = aRobot name
getAttack aRobot =  aRobot attack
getHp aRobot =  aRobot hp
printTuple (name,attack,hp) = "(" ++ show name ++ "," ++ show attack ++ "," ++ show hp ++ ")"
printTuple2 (name,attack,hp) = "robot name: " ++ name ++ ", attack: " ++ show attack ++ ", hp: " ++ show hp
getRobot aRobot = aRobot printTuple
getRobot2 aRobot = aRobot printTuple2


--book version
setName aRobot newName = aRobot (\(n,a,h) -> robot (newName,a,h))
setAttack aRobot newAttack = aRobot (\(n,a,h) -> robot (n,newAttack,h))
setHP aRobot newHP = aRobot (\(n,a,h) -> robot (n,a,newHP))

--damage aRobot attackDamage = aRobot (\(n,a,h) -> robot (n,a,h - attackDamage))
damage aRobot attackDamage = aRobot (\(n,a,h) -> robot (n,a,h - attackDamage))
--damage2 robot aRobot attackDamage = robot (name, attack, hp - attackDamage)


fight aAttacker aDefender = damage aDefender attack
                            where attack = if getHp aAttacker > 0
                                            then getAttack aAttacker
                                            else 0

rJohn = robot ("john",25,200)
rJoe = robot ("joe", 10, 200)
rJack = robot ("jack", 5, 200)
rKiller = robot ("killer", 50, 200)

-- the power of immutability
johnRound3 = fight johnRound2 joeRound2
johnRound2 = fight johnRound1 joeRound1
johnRound1 = fight rJohn rJoe
joeRound3 = fight joeRound2 johnRound2
joeRound2 = fight joeRound1 johnRound1
joeRound1 = fight rJoe rJohn

joeFight = fight rJoe rJohn
johnFight = fight rJohn rJoe
joeResult3 = fight (fight joeFight johnFight) (fight johnFight joeFight)
johnResult3 = fight (fight johnFight joeFight) (fight joeFight johnFight)

-- threeRound robot1 robot2 = 
--                     do
--                       r1Fight <- fight robot1 robot2
--                       r2Fight <- fight robot2 robot1
--                       r1Result <- fight (fight r1Fight r2Fight) (fight r2Fight r1Fight)
--                       r2Result <- fight (fight r2Fight r1Fight) (fight r1Fight r2Fight)
--                       getRobot r1Result ++ getRobot r2Result


threeRound = getRobot (fight (fight r1Fight r2Fight) (fight r2Fight r1Fight)) ++ getRobot (fight (fight r2Fight r1Fight) (fight r1Fight r2Fight))
               where
                r1Fight = fight rJoe rJohn
                r2Fight = fight rJohn rJoe

--threeRoundg robot1 robot2 = let r1Fight = fight robot1 robot2
--                                r2Fight = fight robot2 robot1
--                                in getRobot (fight (fight r1Fight r2Fight) (fight r2Fight r1Fight)) ++ getRobot (fight (fight r2Fight r1Fight) (fight r1Fight r2Fight))


--round robot1 robot2 = fight robot1 robot2 (fight robot2 robot1)
--nRoundFight robot1 robot2 1 = getRobot (fight robot1 robot2) ++ getRobot (fight robot2 robot1)
--nRoundFight robot1 robot2 n = nRoundFight (fight robot1 robot2) (fight robot2 robot1) n-1

--kill = map (getRobot) (map (fight rKiller) [rJoe,rJohn,rJack])
aKill = map (getRobot . fight rKiller) [rJoe,rJohn,rJack]