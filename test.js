import * as script from './script.js'

let failed = false

function assert(condition) {
    failed = failed || !condition
    console.assert(condition)
}

assert(script.permutSign([2, 1, 3], [2, 1, 3]) == 1)

assert(JSON.stringify(script.mergeDeltas([
    [1, 2],
    [3, 4],
])) == JSON.stringify([
    [1, 2],
    [3, 4],
]))

assert(JSON.stringify(script.mergeDeltas([
    [1, 2],
    [3, 4],
    [2, 3],
    [5, 6],
])) == JSON.stringify([
    [1, 2, 3, 4],
    [5, 6],
]))

assert(script.containsDeltas([
    [1, 2, 3, 4],
    [5, 6],
], [[5, 6]]))

assert(script.containsDeltas([
    [1, 2, 3, 4],
    [5, 6],
], [[5, 6]]))

assert(JSON.stringify(script.removeDeltaEntry([
    [1, 2, 3, 4],
    [5, 6],
], 6)) == JSON.stringify([
    [1, 2, 3, 4],
]))

assert(JSON.stringify(script.relatedDelta([
    [1, 2, 3, 4],
    [5, 6],
], 2)) == JSON.stringify([2, 1, 3, 4]))

//////////////////////////////////////////////////////////////////////////////
// Diagram.substitute

const d1 = script.ensureDiagram({
    nodes: [
        script.terminalNode("a", "a", 0, 0),
        script.terminalNode("a", "b", 0, 0),
    ],
    lines: {
        a: script.newLine("a"),
    },
    superlines: {
        a: script.EMPTY_SUPERLINE,
    },
})

script.assertEq(
    new script.Diagram(d1).renameLines({a: "b"}).rawDiagram,
    new script.Diagram(script.ensureDiagram({
        nodes: [
            script.terminalNode("b", "a", 0, 0),
            script.terminalNode("b", "b", 0, 0),
        ],
        lines: {
            b: script.newLine("a"),
        },
        superlines: {
            a: script.EMPTY_SUPERLINE,
        },
    })).rawDiagram)

script.assertEq(
    new script.Diagram().substitute(
        script.EMPTY_DIAGRAM,
        script.EMPTY_DIAGRAM,
    ).rawDiagram,
    script.EMPTY_DIAGRAM,
)

script.assertEq(
    new script.Diagram(d1).substitute(
        script.EMPTY_DIAGRAM,
        script.EMPTY_DIAGRAM,
    ).rawDiagram,
    d1,
)

script.assertEq(new script.Diagram(d1).substitute({
    nodes: [
        script.terminalNode("+a", "x"),
        script.terminalNode("+a", "y"),
    ],
    lines: {
        ["+a"]: {superline: "a", direction: 0},
    },
    superlines: {
        a: script.EMPTY_SUPERLINE,
    },
}, {
    nodes: [
        script.terminalNode("a", "x"),
        script.terminalNode("a", "y"),
    ],
    lines: {
        a: {superline: "a", direction: 0},
    },
    superlines: {
        a: script.EMPTY_SUPERLINE,
    },
}).rawDiagram, d1)

script.assertEq(new script.Diagram(d1).substitute({
    nodes: [
        script.terminalNode("+a", "x"),
        script.terminalNode("+a", "y"),
    ],
    lines: {
        "+a": {superline: "a", direction: 0},
    },
}, {
    nodes: [
        script.terminalNode("$1", "x"),
        script.terminalNode("$2", "y"),
        script.w3jNode("$4", "$4", "$3"),
        script.w3jNode("$3", "$2", "$1"),
    ],
    lines: {
        $4: {superline: "0", direction: +1},
        $3: {superline: "0", direction: 0},
        $2: {superline: "a", direction: 0},
        $1: {superline: "a", direction: +1},
    },
    superlines: {
        a: {weight: 1},
    },
}).rawDiagram, {"nodes":[{"type":"terminal","lines":["4"],"variable":"a","x":0,"y":0},{"type":"terminal","lines":["3"],"variable":"b","x":0,"y":0},{"type":"w3j","lines":["1","1","2"]},{"type":"w3j","lines":["2","3","4"]}],"superlines":{"0":{"phase":0,"summed":false,"weight":0},"a":{"phase":0,"summed":false,"weight":1}},"lines":{"1":{"superline":"0","direction":0,"arrowPos":0.5,"arcHeight":0,"angle":0,"textPos":0.5,"textOffset":0},"2":{"superline":"0","direction":0,"arrowPos":0.5,"arcHeight":0,"angle":0,"textPos":0.5,"textOffset":0},"3":{"superline":"a","direction":0,"arrowPos":0.5,"arcHeight":0,"angle":0,"textPos":0.5,"textOffset":0},"4":{"superline":"a","direction":1,"arrowPos":0.5,"arcHeight":0,"angle":0,"textPos":0.5,"textOffset":0}},"deltas":[]})

if (!failed) {
    document.getElementsByTagName("body")[0].style.background = "black"
}
