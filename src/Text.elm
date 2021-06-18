module Text exposing (controlIntr, getText)

import Msg exposing (..)

story1 =
    "Cosmical Pulse 2077: Green Warrior"
story2 =
    "Good evening to all my fellow students and the executives on board! Group 13 is finally here to entertain you!\nTonight you must be really exhausted so far to maybe revive the softest and gentlest side of humanity by fixing what generates various emotions of all human beings or defend your country from the attack of aliens who covet the GPA of those graduate students, or maybe even make Silverfocs great again with a dedicated investigation into the traitor who revealed the secret of our algorithm to Fudan Inc. \nYou’ve all have done quite an amazing job that got you off the hook and might benefit many at the same time, but your duty tonight is yet to be fulfilled. There are far more people needing you right now than those in this company or school. And the show has just begun…"


story3 =
    "Now, your code name is Kangyu and you are the 114514th space warrior in our secretive scheme to save the universe. But Shhh, don’t tell the person next to you, it’s a secret. \n Whenever you travelled to just now, let’s turn our timer shaft to roughly 50 years from now. Finally, you are heading towards the distant magical 2077, when the advance of technologies is greater than ever…\n The German philosopher Immanuel Kant once noted that “There are only two things I look up to: the moral law within me and the starry heavens above me.”\n Upon your arrival in this magical time travel, it’s a night clearer than crystal by the coast of the English Channel, First thing you notice, Instead of look up to the sky full of stars, people around you seemed to find more enjoyment staying at home and wear a virtual reality goggle where they appreciate the fictitious Aurora in the arctic circle. \n Out of confusion and curiosity, then you look up again, but through a telescope this time, it turned out that those sparkling celestial flashes doesn’t resemble those from the interstellar medium at all, but seem to be given out by those moving objects with more irregular and a bit grotesque shapes."


story4 =
    "With more careful inspection, you found out they are an excessive amount of space junks and debris of deserted space stations all floating in the Asteroid Belt. The universe has been in a state of overload and the waste’s disposal has been overlooked since human beings started to excessively exploit the space resources and perform space station experiments as a trend from the 2050s.\n Sarcastically, to this point, there’s already no sky to be found “full of stars” and absolutely no longer real Aurora in the North Pole which romantic people used to appreciate in awe and make their most beautiful wishes under next to the love of their lives…"


story5 =
    "Nevertheless, there’s still the last pure land in Shanghai, China, the Pearl of the East, which aims to save our once gorgeous universe and the one thing Kant look up to other than his moral rule, the starry heaven. It’s exactly where Cosmical Pulse who made the great spacecraft VG is based. The name of this company inspires our warriors to feel the cosmical pulse like in traditional Chinese medicine and give your diagnosis and subscription. Clearly, the pulse indicates there’s something wrong with the universe right now and it became your job to fix this \n “Welcome to our mission “The Green Warrior”!” A middle-aged but elegant woman and a man of similar age but with a little accent and look exceedingly wise flashed before you, “Please call us Mr and Mrs C. Our company’s goal is to make the world a better place by conducting some amazing work outside THIS world. Yes! You are the chosen one, Kangyu, to guard to last chances of the human race’s faith in the next appearance of Halley’s comet."


story6 =
    "“Now, Will you please step into our Spaceship VG and embark on this magnificent and utterly critical journey to purify the boundless cosmos with us?” asked Mr and Mrs C.\n In the first place, where does all of those space junk come from? You want to find out and alter the status quo. Such a courageous fighter as you has come a long way to this point, so you certainly won't chicken out in front of this challenge, will you? \nWill there still be light at the other end of the tunnel in the future? Will the next year of Halley’s comet return still occur like they expected to be 2137? And most importantly, at the end of the day, will the creatures on Planet Earth still own something to look up to, to hold awe to in the once starry heaven? It’s all up to you. \n Kangyu, your job is to clean all the redundant space junk. Mr And Mrs C expect highly of you and you have large shoes to fill. Baby let the games begin! Good luck with that, because you are going to need it."


story7 =
    "The trash you are dealing with seems to be moving in a rather irregular and random trajectory. High accuracy is entailed. Luckily, you are endowed with the secret weapons to accelerate during the attack and make the efficiency of each bouncing attains its maximum. You can increase the length of the panel which also enlarge the space for the strength accumulation on the spaceship. Besides, you could activate the thruster behind your collector to travel faster! Just increase the speed of the ball! Last, make the panel move faster horizontally so that the collector will have a higher chance of landing back on the optimal area on the spacecraft after an attack. By unlocking the skills Cosmical Pulse has prepared for you with your remaining money, Let’s try what it feels like to launch the collector into different directions by the spaceship you can personally control and see if you get a clearer shot of the trash!  "


die1 =
    "It seems that you have spent all your money Cosmical Pulse has. As a result, the company has also gone bankrupt and this great mission ended up a damp squib. Without exaggeration, It's the darkest day in human history. Still, we sincerely hope that things are going to get better tomorrow should organizations as aspiring and advanced as Cosmical Pulse rise all across the world, in Tokyo, Cape Town, London, New York and Paris... \n All the best wishes to the human race's future and the gorgeous universe surrounding all of us..."


controlIntr =
    "← → move spaceship \n $10 'Q' faster spaceship $20 'E' longer spaceship \n ↑  collector's upward acceleration \n ↓ collector's downward acceleration \n $200 New collector "


getText : Msg -> String
getText msg =
    case msg of
        Game (Story x) ->
            case x of
                1 ->
                    story1

                2 ->
                    story2

                3 ->
                    story3

                4 ->
                    story4

                5 ->
                    story5

                6 ->
                    story6
                7 ->
                    story7

                _ ->
                    ""

        Game Stop ->
            die1

        _ ->
            ""
