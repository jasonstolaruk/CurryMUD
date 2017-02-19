# CurryMUD Cheatsheet

Example command output assumes your character's name is Hanako.

## Abbreviation

Nearly all identifiers in CurryMUD may be abbreviated. This means that in many 
cases you need not type the entire name of an item, a character, a command, or 
a help article.

## Random Names

In channel communications, randomly chosen nouns identify characters with whom you have not established a telepathic link. These "random names" are easily distinguished by the fact that they always start with a lowercase letter and appear underlined. See `help randomnames` for more info.

## Listing And Examining Things

Command | Example | Result
--- | --- | ---
`look` | `look` | get a description of your current room, including a list of things in the room
| | `look mhuman` | look at the male human in the room
`inventory` | `inventory` | get a list of the items you're carrying
| | `inventory apple` | examine the apple you're carrying
`equipment` | `equipment` | get a list of your readied equipment
| | `equipment sword` | examine the sword you're wielding

<div style="page-break-after: always;"></div>
## Location Prefixes

Prefix | Location | Example | Result
--- | --- | --- | ---
`i-` | inventory | `smell i-flask` | smell the (contents of the) flask in your inventory
`e-` | equipment | `smell e-boots` | smell the boots you're wearing
`r-` | room | `smell r-flower` | smell the flower in the room

## Other Prefixes

Prefix | Purpose | Example | Result
--- | --- | --- | ---
`.` | ordinal number | `empty 3.bottle` | empty your third bottle
`/` | amount | `get 2/knife` | pick up the first two knives in the room
`'` | all | `show 'earring taro` | show all of the earrings in your inventory to Taro
| | | `drop '` | drop everything in your inventory (including coins)

## Combining Prefixes

You may combine a location prefix with another prefix. The location prefix must 
come first.

Example | Result
--- | ---
`smell r-2/flower` | smell the first two flowers in the room
`show e-'earring taro` | show all the earrings you're wearing to Taro

<div style="page-break-after: always;"></div>
## Coins

There are `cp` (copper pieces), `sp` (silver pieces), and `gp` (gold pieces).

cp | sp | gp
--- | --- | ---
1 cp | 1    | 1/10 | 1/100
1 sp | 10   | 1    | 1/10
1 gp | 100  | 10   | 1

## Prefixes Used With Coins

Prefix | Purpose | Example | Result
--- | --- | --- | ---
`/` | amount | `put 50/gp sack` | put 50 of your gold pieces in your sack
| | | `remove 50/coins sack` | remove 50 coins from your sack
`'` | all | `put 'gp sack` | put all of your gold pieces in your sack
| | | `remove 'coins sack` | remove all of the coins from your sack

<div style="page-break-after: always;"></div>
## Emoting

Symbol | Purpose | Example | Output
--- | --- | --- | ---
`@` | your name | `emote shielding her`</br>`eyes from the sun, @`</br>`looks out across the`</br>`plains` | [Shielding her eyes from the sun, Hanako looks out across the plains.]
`@'s` | your name in possessive | `@'s leg twitches`</br>`involuntarily as she`</br>` laughs with gusto` | [Hanako's leg twitches involuntarily as she laughs with gusto.]
`>taro` | target Taro | `emote slowly turns`</br>`her head to look`</br>`directly at >taro` | [Hanako slowly turns her head to look directly at Taro.]
`>fhobbit's` | target the female hobbit, in possessive | `emote places her`</br>`hand firmly on`</br>`>fhobbit's shoulder` | [Hanako places her hand firmly on the female hobbit's shoulder.]
| | | `emote ignoring`</br>`>mnymph, @ takes`</br>`>taro's hand and`</br>`gestures eastward` | [Ignoring the male nymph, Hanako takes Taro's hand and gestures eastward.]

<div style="page-break-after: always;"></div>
## Emotes And Expressive Commands Inside Other Commands

Symbol | Purpose | Example | Output
--- | --- | --- | ---
`;` | begin an emote | `telepathy taro ;gives`</br>`you her full attention` | [Hanako] [Hanako gives you her full attention.]
| | | `channel hunt ;suggests`</br>`that >tomato stay put`</br>`for now` | (hunt) Hanako: [Hanako suggests that __tomato__ stay put for now.]
| | | `question ;throws her`</br>`arms up in exasperation` | (Question) Hanako: [Hanako throws her arms up in exasperation.]
`=` | begin an expressive command | `telepathy taro =nodagree`</br>`taro` | [Hanako] [Hanako nods to you in agreement.]
| | | `channel hunt =slowclap`</br>`wellwater` | (hunt) Hanako: \<With a mocking lack of enthusiasm, Hanako claps slowly for __wellWater__.\>
| | | `question =comfort taro` | (Question) Hanako: \<Hanako comforts Taro.\>

<div style="page-break-after: always;"></div>
## Addressing A Message

Use `>` to address a message.

Example | Output
--- | ---
`say >taro nice to mee, you, Taro` | Hanako says to Taro, "Nice to meet you, Taro."
`felinoidean >taro he can't be trusted` | Hanako says to Taro in felinoidean, "He can't be trusted."
`channel hunt >taro i'm still waiting`</br>`in the clearing` | (hunt) Hanako: (to Taro) I'm still waiting in the clearing.
`question >ant try typing "help`</br>`cheatsheet"` | (Question) Hanako: (to __ant__) Try typing "help cheatsheet".

## Adverbial Phrases

You may use an adverbial phrase to describe how an utterance is spoken. Adverbial phrases are delimited by square brackets.

Example | Output
--- | ---
`say [in a high-pitched voice] stop`</br>`pressuring me!` | Hanako says in a high-pitched voice, "Stop pressuring me!"
`say [quietly] >taro she's completely`</br>`round the twist...` | Hanako says quietly to Taro, "She's completely round the twist..."
`say >taro [with some hesitation] i`</br>`suppose you're right` | Hanako says to Taro with some hesitation, "I suppose you're right."
`dwarvish [irately] now fetch me`</br>`another beer!` | Hanako says irately in dwarvish, "Now fetch me another beer!"

<div style="page-break-after: always;"></div>
## Room Fixtures

A "room fixture" is a permanent feature of a room (such as a sign) with which 
you can interact via one or more commands. **You may not abbreviate the name of a room fixture, and you may need to tack on the `r-` prefix to indicate that you are targeting an object in your current room.**

## Undocumented Commands

There do exist commands for which there is no help available via the `help` and `?` commands. Typically, undocumented commands may only be used in certain rooms. One example is the `trash` command, which only works when you are in a room with a trash bin. **The names of undocumented commands may not be abbreviated.**
