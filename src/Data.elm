module Data exposing
    ( Alignment
    , Card
    , EventCard
    , Faction
    , TypeMeta
    , alignments
    , cardById
    , cards
    , disclaimer
    , eventById
    , events
    , factionById
    , factions
    , opposites
    , subtitle
    , title
    , typeMeta
    )

title : String
title =
    "Takeoff Window"


subtitle : String
subtitle =
    "The last five years, played in back rooms."


disclaimer : String
disclaimer =
    "A work of satire. Every faction, figure, and headline is a fictionalized parody. Nothing here is a factual claim about any real person or organization."


type alias Faction =
    { id : String
    , name : String
    , short : String
    , motto : String
    , color : String
    , power : Int
    , income : Int
    , sockets : Int
    , flavor : String
    , ability : String
    , goalText : String
    , goalHint : String
    }


type alias Card =
    { id : String
    , name : String
    , title : String
    , type_ : String
    , traits : List String
    , parody : Bool
    , power : Int
    , resistance : Int
    , transfer : Int
    , income : Int
    , compute : Int
    , alignments : List String
    , sockets : Int
    , special : Maybe String
    , flavor : String
    }


type alias EventCard =
    { id : String
    , name : String
    , flavor : String
    }


type alias Alignment =
    { color : String
    , letter : String
    }


type alias TypeMeta =
    { label : String
    , color : String
    }


factions : List Faction
factions =
    [     { id = "helios"
    , name = "Helios Dynamics"
    , short = "Helios"
    , motto = "Ship it. Scale it. Light the sky."
    , color = "#f0a020"
    , power = 9
    , income = 4
    , sockets = 3
    , flavor = "A frontier lab convinced that every safety memo is a latency bug and that the only real alignment is a loss curve going down."
    , ability = "+1 power when attacking Labs or Chipmakers."
    , goalText = "Have 20 or more Compute, or hold a settled Lab and a settled Chipmaker with at least 12 Compute."
    , goalHint = "Win the compute race at 20, or settle a lab and a chipmaker and reach 12."
    }
    ,     { id = "open-wave"
    , name = "The Open Wave"
    , short = "Open Wave"
    , motto = "Weights want to be free."
    , color = "#4ec4d4"
    , power = 10
    , income = 4
    , sockets = 3
    , flavor = "Accelerationists, forum legends, and a few sincere librarians of weights. They believe a closed model is a coup waiting to happen."
    , ability = "+2 power when attacking Open-aligned groups. You may attempt to leak weights if you control a Lab."
    , goalText = "A Weights Leak has happened and you hold three settled Open-aligned groups — or you hold the Open Weights Collective and four settled groups."
    , goalHint = "Open the models, then own the people who distribute them."
    }
    ,     { id = "pause"
    , name = "The Pause Compact"
    , short = "Pause"
    , motto = "Slow is smooth. Smooth is survival."
    , color = "#6fc9a0"
    , power = 9
    , income = 4
    , sockets = 3
    , flavor = "A coalition of longtermists, letter-signers, and professional worriers. They would like the future to arrive, just not this Tuesday."
    , ability = "+3 power on Neutralize or Destroy attacks against Frontier groups."
    , goalText = "Destroy or neutralize three Frontier groups that were under someone's control, or hold a settled Regulator, a settled Media group, and any third settled group."
    , goalHint = "Pull the labs off the board, or own the referee and the headline."
    }
    ,     { id = "directorate"
    , name = "Directorate 7"
    , short = "Directorate"
    , motto = "National security is a kind of safety."
    , color = "#d4785a"
    , power = 10
    , income = 3
    , sockets = 3
    , flavor = "A national-security apparatus that has discovered \"foundation models\" and would prefer the founders not make any more sudden trips."
    , ability = "+2 power when attacking Founder or Lab groups."
    , goalText = "Hold a settled Frontier group, a settled Regulator, and three settled groups in total — or four settled groups including two Nationalist ones."
    , goalHint = "A lab in one pocket, a regulator in the other — or a four-group patriotic stack."
    }
    ,     { id = "long-plan"
    , name = "The Long Plan"
    , short = "Long Plan"
    , motto = "The count is taken in decades, and in wafers."
    , color = "#e05d5d"
    , power = 9
    , income = 4
    , sockets = 3
    , flavor = "A sovereign compute bloc with a very patient org chart. Export controls are treated as a weather report."
    , ability = "Chipmakers you control produce +1 income. You ignore Export Control penalties."
    , goalText = "Control a Chipmaker, a Hyperscaler, and three other groups — or have at least 24 Compute and more Compute than every rival."
    , goalHint = "Own the fab, the cloud, and a wider stack — or simply out-compute the rest of the planet."
    }
    ,     { id = "crown"
    , name = "Crown Sovereign"
    , short = "Crown"
    , motto = "Megawatts first. Models follow."
    , color = "#c9a227"
    , power = 9
    , income = 5
    , sockets = 3
    , flavor = "A sovereign buyer with a blank check and a preference for owning the physical stack: power, land, and the chips that sit on both."
    , ability = "Power and Datacenter groups you control produce +1 income. +2 power when attacking Power, Datacenter, or Chipmaker groups."
    , goalText = "Hold a settled Power group, a settled Datacenter, and a settled Chipmaker — or have 64 money and three settled groups."
    , goalHint = "Buy the grid, the building, and the silicon. Or just be unimaginably liquid."
    }
    ]


cards : List Card
cards =
    [     { id = "green-jacket"
    , name = "Green Jacket"
    , title = "Keynote Sovereign"
    , type_ = "chipmaker"
    , traits = [ "Compute", "Founder" ]
    , parody = True
    , power = 6
    , resistance = 5
    , transfer = 5
    , income = 4
    , compute = 3
    , alignments = [ "Corporate", "Accelerationist" ]
    , sockets = 2
    , special = Nothing
    , flavor = "He has worn the same ceremonial jacket to every keynote since the first training run that mattered. The demos are always \"just around the corner,\" and the corner keeps arriving."
    }
    ,     { id = "island-foundry"
    , name = "Island Foundry"
    , title = "The Only Island That Matters"
    , type_ = "chipmaker"
    , traits = [ "Compute" ]
    , parody = False
    , power = 5
    , resistance = 7
    , transfer = 4
    , income = 3
    , compute = 2
    , alignments = [ "Corporate", "Nationalist" ]
    , sockets = 1
    , special = Nothing
    , flavor = "A foundry on an island that everyone has a plan for and no one wants to test. The wafers come out; the speeches stay in."
    }
    ,     { id = "memory-cartel"
    , name = "Memory Cartel"
    , title = "HBM Allocation Desk"
    , type_ = "chipmaker"
    , traits = [ "Compute" ]
    , parody = False
    , power = 4
    , resistance = 5
    , transfer = 3
    , income = 3
    , compute = 1
    , alignments = [ "Corporate" ]
    , sockets = 1
    , special = Nothing
    , flavor = "They do not make the brains. They make the part the brains cannot think without, and they invoice accordingly."
    }
    ,     { id = "rivercloud"
    , name = "Rivercloud"
    , title = "The Default Cloud"
    , type_ = "hyperscaler"
    , traits = [ "Compute" ]
    , parody = False
    , power = 5
    , resistance = 6
    , transfer = 5
    , income = 3
    , compute = 2
    , alignments = [ "Corporate" ]
    , sockets = 2
    , special = Nothing
    , flavor = "If your model lives anywhere, it probably lives here, in a region with a name like a suburb and a power bill like a nation."
    }
    ,     { id = "search-temple"
    , name = "Search Temple"
    , title = "The Campus With the Cafeteria"
    , type_ = "hyperscaler"
    , traits = [ "Compute" ]
    , parody = False
    , power = 5
    , resistance = 6
    , transfer = 4
    , income = 3
    , compute = 2
    , alignments = [ "Corporate", "Academic" ]
    , sockets = 2
    , special = Nothing
    , flavor = "They invented half the ideas, staffed both sides of the argument, and still ship the model under a different letterhead."
    }
    ,     { id = "conglomerate"
    , name = "The Conglomerate"
    , title = "Mixed-Use Software Empire"
    , type_ = "hyperscaler"
    , traits = [ "Compute" ]
    , parody = False
    , power = 6
    , resistance = 6
    , transfer = 5
    , income = 3
    , compute = 2
    , alignments = [ "Corporate" ]
    , sockets = 2
    , special = Nothing
    , flavor = "An operating-system dynasty that woke up one morning owning the lab, the cloud, and the office suite the lab writes memos in."
    }
    ,     { id = "the-campus"
    , name = "The Campus"
    , title = "Engagement Hyperscaler"
    , type_ = "hyperscaler"
    , traits = [ "Compute" ]
    , parody = False
    , power = 4
    , resistance = 5
    , transfer = 3
    , income = 3
    , compute = 1
    , alignments = [ "Corporate", "Chaotic" ]
    , sockets = 1
    , special = Nothing
    , flavor = "A walled garden with a research lab in the basement and a feed upstairs that would like a word with that lab."
    }
    ,     { id = "first-letter"
    , name = "The First Letter"
    , title = "Frontier Lab, First Mover"
    , type_ = "lab"
    , traits = [ "Frontier", "Compute" ]
    , parody = True
    , power = 5
    , resistance = 5
    , transfer = 4
    , income = 2
    , compute = 2
    , alignments = [ "Accelerationist", "Corporate" ]
    , sockets = 2
    , special = Nothing
    , flavor = "They promised a charter, a commission, and a carefully staged departure from \"run the lab like a startup.\" The demo still shipped on a Tuesday."
    }
    ,     { id = "constitutional"
    , name = "Constitutional Lab"
    , title = "The Careful Ones"
    , type_ = "lab"
    , traits = [ "Frontier", "Compute" ]
    , parody = False
    , power = 4
    , resistance = 6
    , transfer = 3
    , income = 2
    , compute = 1
    , alignments = [ "Safety", "Academic" ]
    , sockets = 1
    , special = Nothing
    , flavor = "They write the model a constitution and then argue, at impressive length, about whether it has read it."
    }
    ,     { id = "the-garden"
    , name = "The Garden"
    , title = "The Original Research Orchard"
    , type_ = "lab"
    , traits = [ "Frontier", "Compute" ]
    , parody = False
    , power = 5
    , resistance = 5
    , transfer = 4
    , income = 2
    , compute = 2
    , alignments = [ "Academic" ]
    , sockets = 1
    , special = Nothing
    , flavor = "Games, proteins, and a quiet sense that they were here first. The parent company would prefer a product."
    }
    ,     { id = "fast-follow"
    , name = "The Fast Follow"
    , title = "The Other Launch"
    , type_ = "lab"
    , traits = [ "Frontier", "Founder", "Compute" ]
    , parody = True
    , power = 5
    , resistance = 4
    , transfer = 4
    , income = 2
    , compute = 2
    , alignments = [ "Accelerationist", "Chaotic" ]
    , sockets = 1
    , special = Nothing
    , flavor = "A lab, a rocket, a car, and a feed, all somehow the same calendar invite. The training run is always \"this weekend.\""
    }
    ,     { id = "open-weights"
    , name = "Open Weights Collective"
    , title = "The Fork That Would Not Die"
    , type_ = "lab"
    , traits = [ "OpenSource", "Compute" ]
    , parody = False
    , power = 3
    , resistance = 4
    , transfer = 3
    , income = 1
    , compute = 1
    , alignments = [ "Open", "Accelerationist" ]
    , sockets = 1
    , special = Nothing
    , flavor = "A distributed choir of people who will, if necessary, train the model in a group chat and post the result under a license no lawyer enjoys."
    }
    ,     { id = "regional-grid"
    , name = "Regional Grid Authority"
    , title = "The Megawatt Gate"
    , type_ = "power"
    , traits = [ "Power" ]
    , parody = False
    , power = 3
    , resistance = 6
    , transfer = 2
    , income = 2
    , compute = 0
    , alignments = [ "Nationalist", "Corporate" ]
    , sockets = 1
    , special = Nothing
    , flavor = "They do not care about your loss function. They care about the interconnect queue, and the queue is three years long."
    }
    ,     { id = "smr-promise"
    , name = "Small Modular Promise"
    , title = "The Reactor That Is Coming"
    , type_ = "power"
    , traits = [ "Power" ]
    , parody = False
    , power = 2
    , resistance = 4
    , transfer = 2
    , income = 2
    , compute = 0
    , alignments = [ "Academic" ]
    , sockets = 1
    , special = Nothing
    , flavor = "A beautiful slide deck of a reactor, perpetually eighteen months from a pour of concrete. Investors call this \"optionality.\""
    }
    ,     { id = "interconnect"
    , name = "The Interconnect Queue"
    , title = "Permission to Plug In"
    , type_ = "power"
    , traits = [ "Power" ]
    , parody = False
    , power = 2
    , resistance = 7
    , transfer = 1
    , income = 1
    , compute = 0
    , alignments = [ "Corporate" ]
    , sockets = 0
    , special = Just "controller-power-income"
    , flavor = "The most powerful document in the industry is a place in line. Controlling the queue is almost like controlling the grid."
    }
    ,     { id = "model-bureau"
    , name = "Bureau of Model Safety"
    , title = "The New Referee"
    , type_ = "regulator"
    , traits = [ "Regulator" ]
    , parody = False
    , power = 4
    , resistance = 6
    , transfer = 3
    , income = 1
    , compute = 0
    , alignments = [ "Safety" ]
    , sockets = 1
    , special = Nothing
    , flavor = "A freshly minted office with a seal, a framework, and a sincere hope that the models will wait for the comment period."
    }
    ,     { id = "export-desk"
    , name = "Export Control Desk"
    , title = "The License to Ship"
    , type_ = "regulator"
    , traits = [ "Regulator" ]
    , parody = False
    , power = 5
    , resistance = 6
    , transfer = 3
    , income = 1
    , compute = 0
    , alignments = [ "Nationalist" ]
    , sockets = 1
    , special = Nothing
    , flavor = "They decide which wafers may travel. Entire national strategies have been reduced to a footnote in their guidance."
    }
    ,     { id = "subcommittee"
    , name = "Senate Subcommittee"
    , title = "The Hearing Room"
    , type_ = "regulator"
    , traits = [ "Regulator" ]
    , parody = False
    , power = 3
    , resistance = 5
    , transfer = 2
    , income = 1
    , compute = 0
    , alignments = [ "Nationalist", "Chaotic" ]
    , sockets = 1
    , special = Nothing
    , flavor = "Founders are invited to explain, under oath and under lights, why the demo should be allowed to finish talking."
    }
    ,     { id = "the-timeline"
    , name = "The Timeline"
    , title = "The Global Group Chat"
    , type_ = "media"
    , traits = [ "Media" ]
    , parody = True
    , power = 4
    , resistance = 3
    , transfer = 3
    , income = 2
    , compute = 0
    , alignments = [ "Chaotic", "Open" ]
    , sockets = 1
    , special = Nothing
    , flavor = "A public square owned by a man who also owns a lab. Consensus forms here in minutes and collapses in the replies."
    }
    ,     { id = "prestige-mag"
    , name = "Prestige Magazine"
    , title = "The Long Profile"
    , type_ = "media"
    , traits = [ "Media" ]
    , parody = False
    , power = 3
    , resistance = 4
    , transfer = 2
    , income = 1
    , compute = 0
    , alignments = [ "Academic", "Safety" ]
    , sockets = 1
    , special = Nothing
    , flavor = "A glossy that can make a founder a statesman or a cautionary tale, sometimes in the same paragraph."
    }
    ,     { id = "podcast-circuit"
    , name = "The Podcast Circuit"
    , title = "Three Hours, No Notes"
    , type_ = "media"
    , traits = [ "Media" ]
    , parody = False
    , power = 3
    , resistance = 3
    , transfer = 2
    , income = 1
    , compute = 0
    , alignments = [ "Chaotic" ]
    , sockets = 1
    , special = Nothing
    , flavor = "Long conversations in which AGI is declared inevitable, then delayed, then inevitable again, with ads for mattresses."
    }
    ,     { id = "soft-landing"
    , name = "Soft Landing Capital"
    , title = "The Responsible Fund"
    , type_ = "vc"
    , traits = [ "VC" ]
    , parody = False
    , power = 3
    , resistance = 4
    , transfer = 3
    , income = 3
    , compute = 0
    , alignments = [ "Corporate", "Safety" ]
    , sockets = 1
    , special = Nothing
    , flavor = "They would like to be rich and also invited to the safety dinner. The term sheet has a morals clause, and a ratchet."
    }
    ,     { id = "hundred-b"
    , name = "The Hundred-Billion Fund"
    , title = "The Blank Check"
    , type_ = "vc"
    , traits = [ "VC" ]
    , parody = False
    , power = 4
    , resistance = 4
    , transfer = 4
    , income = 4
    , compute = 0
    , alignments = [ "Corporate", "Accelerationist" ]
    , sockets = 2
    , special = Nothing
    , flavor = "A fund the size of a sovereign that exists to make sure no training run ever pauses for \"just a quarter.\""
    }
    ,     { id = "desert-builders"
    , name = "Desert Campus Builders"
    , title = "Concrete and Coolant"
    , type_ = "datacenter"
    , traits = [ "Datacenter", "Compute" ]
    , parody = False
    , power = 3
    , resistance = 5
    , transfer = 2
    , income = 2
    , compute = 1
    , alignments = [ "Corporate" ]
    , sockets = 1
    , special = Nothing
    , flavor = "They pour slabs in places named after lakes that are no longer there. The clusters come online one transformer at a time."
    }
    ,     { id = "colo-saints"
    , name = "The Colocation Saints"
    , title = "Racks for Hire"
    , type_ = "datacenter"
    , traits = [ "Datacenter", "Compute" ]
    , parody = False
    , power = 3
    , resistance = 4
    , transfer = 2
    , income = 2
    , compute = 1
    , alignments = [ "Corporate" ]
    , sockets = 1
    , special = Nothing
    , flavor = "A quiet order of people who can find you five megawatts by Thursday, if you do not ask where from."
    }
    ,     { id = "care-forum"
    , name = "The Care Forum"
    , title = "The Long Essay Factory"
    , type_ = "thinktank"
    , traits = [ "ThinkTank" ]
    , parody = False
    , power = 2
    , resistance = 4
    , transfer = 2
    , income = 1
    , compute = 0
    , alignments = [ "Safety", "Academic" ]
    , sockets = 1
    , special = Nothing
    , flavor = "A forum of careful people writing careful posts about extinction, funding, and who is allowed to feel normal."
    }
    ,     { id = "accel-institute"
    , name = "Acceleration Institute"
    , title = "The Opposite Newsletter"
    , type_ = "thinktank"
    , traits = [ "ThinkTank" ]
    , parody = False
    , power = 2
    , resistance = 3
    , transfer = 2
    , income = 1
    , compute = 0
    , alignments = [ "Accelerationist", "Open" ]
    , sockets = 1
    , special = Nothing
    , flavor = "They publish charts that only go up, and treat a pause as a kind of heresy with worse footnotes."
    }
    ,     { id = "poach-list"
    , name = "The Poach List"
    , title = "Talent Brokerage"
    , type_ = "talent"
    , traits = [ "Talent" ]
    , parody = False
    , power = 3
    , resistance = 3
    , transfer = 2
    , income = 1
    , compute = 0
    , alignments = [ "Chaotic" ]
    , sockets = 1
    , special = Just "founder-hunter"
    , flavor = "A spreadsheet of researchers and the number that would move them. Founders are just rows with higher numbers."
    }
    ,     { id = "minister-chips"
    , name = "Minister of Chips"
    , title = "Industrial Policy, In Person"
    , type_ = "talent"
    , traits = [ "Founder", "Talent" ]
    , parody = False
    , power = 4
    , resistance = 5
    , transfer = 3
    , income = 2
    , compute = 0
    , alignments = [ "Nationalist" ]
    , sockets = 1
    , special = Nothing
    , flavor = "A minister who has discovered that \"semiconductor sovereignty\" photographs well, and occasionally even funds a fab."
    }
    ,     { id = "listening-service"
    , name = "The Listening Service"
    , title = "The Quiet Instrument"
    , type_ = "regulator"
    , traits = [ "Regulator" ]
    , parody = False
    , power = 5
    , resistance = 6
    , transfer = 3
    , income = 1
    , compute = 0
    , alignments = [ "Nationalist", "Chaotic" ]
    , sockets = 1
    , special = Nothing
    , flavor = "A desk that does not appear on org charts. Officially they write briefings. Unofficially, a founder can be invited to a meeting and come back without a company."
    }
    ,     { id = "benchmark-cmte"
    , name = "Benchmark Committee"
    , title = "The Scorekeepers"
    , type_ = "thinktank"
    , traits = [ "ThinkTank" ]
    , parody = False
    , power = 2
    , resistance = 5
    , transfer = 1
    , income = 1
    , compute = 0
    , alignments = [ "Academic" ]
    , sockets = 0
    , special = Nothing
    , flavor = "They decide what \"smarter\" means this quarter. Entire labs rearrange their roadmaps around a leaderboard these people host."
    }
    ]


events : List EventCard
events =
    [     { id = "plant-delay", name = "Power Plant Delay", flavor = "A turbine shipment is stuck in a port, and a campus that was \"energizing in Q3\" is not." }
    ,     { id = "export-controls", name = "Export Controls", flavor = "A new rule appears on a Friday afternoon. Several procurement teams cancel dinner." }
    ,     { id = "gpu-shortage", name = "GPU Shortage", flavor = "The allocation meeting lasts six hours. Someone uses the word \"fairness,\" and is ignored." }
    ,     { id = "boardroom-coup", name = "Boardroom Coup", flavor = "A founder is invited to a meeting about \"governance\" and leaves without the badge." }
    ,     { id = "weights-leak", name = "Weights Leak", flavor = "A torrent appears. For a few hours, half the industry pretends not to have clicked it." }
    ,     { id = "cluster-fire", name = "Cluster Fire", flavor = "A cooling line fails at 3 a.m. The incident report will use the word \"thermal.\"" }
    ,     { id = "talent-exodus", name = "Talent Exodus", flavor = "A well-timed poaching wave leaves a lab with a lot of empty standing desks and a statement about \"focus.\"" }
    ,     { id = "safety-letter", name = "The Safety Letter", flavor = "Another open letter. This one has more signatures, better stationery, and a week of coverage." }
    ,     { id = "funding-round", name = "Funding Round", flavor = "Someone raises at a number that makes last year's number look shy." }
    ,     { id = "hearing", name = "Congressional Hearing", flavor = "The chair wants a yes-or-no answer on whether the model is \"conscious, or close.\"" }
    ,     { id = "eval-scoop", name = "Evaluation Scoop", flavor = "A benchmark number leaks early. The lab in question neither confirms nor sleeps." }
    ,     { id = "interconnect-denial", name = "Interconnect Denial", flavor = "The utility says no, politely, in a letter that will be cited for the next decade." }
    ,     { id = "open-drop", name = "Open-Source Drop", flavor = "A nearly-frontier model lands on a public repo with a README that just says \"have fun.\"" }
    ,     { id = "emergency-powers", name = "Emergency Powers", flavor = "A memo is signed. Several agencies discover they always had this authority." }
    ,     { id = "subsidy", name = "Industrial Subsidy", flavor = "A grant the size of a mid-tier lab is awarded to whoever already had the land." }
    ,     { id = "flash-crash", name = "Flash Crash", flavor = "A paper selloff. Treasuries are suddenly not as liquid as the slide deck claimed." }
    ,     { id = "demo-day", name = "Model Demo Day", flavor = "The livestream stutters, then does something no one on the panel can quite explain." }
    ,     { id = "visa-freeze", name = "Visa Freeze", flavor = "A researcher's start date moves from Monday to \"we will be in touch.\"" }
    ,     { id = "good-news", name = "A Quiet Quarter", flavor = "For once, nothing catches fire, leaks, or testifies. The work continues." }
    ,     { id = "grid-upgrade", name = "Grid Upgrade", flavor = "A substation comes online early. Someone in a hard hat cuts a ribbon no model will remember." }
    ]


alignments : List ( String, Alignment )
alignments =
    [ ( "Corporate", { color = "#c9a227", letter = "C" } )
    , ( "Open", { color = "#4ec4d4", letter = "O" } )
    , ( "Safety", { color = "#6fc9a0", letter = "S" } )
    , ( "Nationalist", { color = "#d4785a", letter = "N" } )
    , ( "Accelerationist", { color = "#e0873d", letter = "A" } )
    , ( "Academic", { color = "#8ba0c9", letter = "K" } )
    , ( "Chaotic", { color = "#c97bb0", letter = "X" } )
    ]


opposites : List ( String, String )
opposites =
    [ ( "Corporate", "Open" )
    , ( "Open", "Corporate" )
    , ( "Safety", "Accelerationist" )
    , ( "Accelerationist", "Safety" )
    , ( "Nationalist", "Chaotic" )
    , ( "Chaotic", "Nationalist" )
    ]


typeMeta : List ( String, TypeMeta )
typeMeta =
    [ ( "chipmaker", { label = "Chipmaker", color = "#e0b44a" } )
    , ( "hyperscaler", { label = "Hyperscaler", color = "#5b8def" } )
    , ( "lab", { label = "Lab", color = "#9b7dff" } )
    , ( "power", { label = "Power", color = "#8fd45a" } )
    , ( "regulator", { label = "Regulator", color = "#d46b4c" } )
    , ( "media", { label = "Media", color = "#e07aa8" } )
    , ( "vc", { label = "Venture", color = "#dbbf5a" } )
    , ( "datacenter", { label = "Datacenter", color = "#4ec4d4" } )
    , ( "thinktank", { label = "Think Tank", color = "#8b8796" } )
    , ( "talent", { label = "Talent", color = "#e6e2d6" } )
    ]


factionById : String -> Faction
factionById id =
    factions
        |> List.filter (\f -> f.id == id)
        |> List.head
        |> Maybe.withDefault (fallbackFaction id)


cardById : String -> Card
cardById id =
    cards
        |> List.filter (\c -> c.id == id)
        |> List.head
        |> Maybe.withDefault (fallbackCard id)


eventById : String -> EventCard
eventById id =
    events
        |> List.filter (\e -> e.id == id)
        |> List.head
        |> Maybe.withDefault { id = id, name = id, flavor = "" }


fallbackFaction : String -> Faction
fallbackFaction id =
    { id = id
    , name = id
    , short = id
    , motto = ""
    , color = "#e0b44a"
    , power = 8
    , income = 3
    , sockets = 3
    , flavor = ""
    , ability = ""
    , goalText = ""
    , goalHint = ""
    }


fallbackCard : String -> Card
fallbackCard id =
    { id = id
    , name = id
    , title = ""
    , type_ = "lab"
    , traits = []
    , parody = False
    , power = 1
    , resistance = 1
    , transfer = 0
    , income = 0
    , compute = 0
    , alignments = []
    , sockets = 0
    , special = Nothing
    , flavor = ""
    }
