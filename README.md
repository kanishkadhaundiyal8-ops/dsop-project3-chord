

Chord P2P Simulation in Gleam
Overview
This project is an implementation of the Chord peer-to-peer (P2P) protocol, written in the Gleam programming language and utilizing the Actor model for concurrency.

The simulation dynamically constructs a P2P overlay network where nodes can join to form a ring structure. It models key-value storage by routing lookup requests according to the Chord algorithm. The primary goal is to analyze the routing efficiency of the network. After a series of random lookups, the program calculates and reports the average number of hops required to resolve a request.

Core Features
Actor-Based Design: Every peer in the Chord network is implemented as a distinct and independent actor process.

Dynamic Network Formation: Implements the Chord protocol, allowing nodes to properly join and form the P2P ring.

Key-Value Service: Offers a basic lookup service for string data that is mapped to integer keys.

Performance Analysis: The simulation concludes by outputting the average hop count across all lookup operations, providing a key performance metric.

Usage
Follow these steps to compile and run the simulation.

1. Build the Project
First, ensure all dependencies are downloaded and the project is built by running the following command in your terminal:

Shell

gleam build
2. Run the Simulation
Once the project is built, execute the main module with this command:

Shell

gleam run --module main
3. Provide Input
After running the command, the program will prompt you to enter the simulation parameters directly into your terminal. Provide the desired number of nodes and requests, then press Enter.

For example:

Plaintext

Enter the number of nodes: 100
Enter the number of requests per node: 10

Initializing Chord P2P Network Simulation...
Network Size: 100 nodes, 10 requests each.
...