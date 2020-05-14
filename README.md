
# Planning Poker

## Delphi App to send Sprint Planning Estimates remotely

[Download Planning Poker v1.2.1](https://github.com/digao-dalpiaz/Planning-Poker/releases/download/v1.2.1/PlanningPoker.exe)

## What's New

- 05/13/2020 (Version 1.3.0)

   - Language support (English and Portuguese-Brazil languages).
   - Implemented Abstain button, to send an estimate with zero value.
   - Auto-close round when all clients have estimate done.

## Description

With this App your team can estimate Sprint task remotely.
Working as client/server, your team needs to choose one member to be the Server, and the others will be clients, just like in a Host/Client game.

The Server and Client App are the same exe, just selecting the operation mode in the Welcome page.

The App uses my Socket component **DzSocket**.
You can get it here: https://github.com/digao-dalpiaz/DzSocket

It works by default on TCP Port 6696. Of course, you can change this port in the source code.

You can use in Local Network and/or Internet environment. To use through the Internet, the server side needs to open TCP port, allowing incoming clients connections to the server.

The operation is quite simple: the users just fill their names and connect to the server. Then the server operator can open a round, when the users will be able to send their estimates. At this time, no one can see the numbers, neither the server. When the estimates are completed, the server operator can close the round, and then the numbers and statistics will be revealed to everyone. :grin:

Start page:

![Start Tab](images/start_tab.png)

Poker page when round is opened:

![Poker Tab Opened](images/poker_tab_opened.png)

Poker page when round is closed:

![Poker Tab Closed](images/poker_tab_closed.png)
