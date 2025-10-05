
This project provides an implementation of the Chord peer-to-peer (P2P) protocol, developed in the Gleam programming language and built upon the Actor model.

The simulation constructs a P2P overlay network, allowing nodes to dynamically join and form a ring structure. It faithfully models key-value storage lookups by routing requests through the network according to the Chord algorithm. Each peer in the network is an independent actor, managing its own state.

The primary goal of this simulation is to analyze the efficiency of Chord's routing. After running a series of random lookups initiated by each node, the program calculates and reports the average number of hops required to resolve a request.

**Functionality:**

-   **Actor-Based Peers:** Every node in the Chord network is a distinct actor process.
-   **Network Formation:** Implements the Chord protocol for nodes joining the P2P ring.
-   **Key-Value Service:** Provides a basic lookup service for string data mapped to integer keys.
-   **Performance Metrics:** The simulation concludes by outputting the average hop count for all lookup operations.

**Execution:**

The project can be run from the command line, specifying the number of nodes and the number of requests each node should perform:

`project3 numNodes numRequests`
