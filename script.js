"use strict";

//////////////////////////////////////////////////////////////////////////////
// Utility

// Floored modulo
function mod(x, y) {
    return (x % y + y) % y;
}

function sortTwo(x, y) {
    if (x < y) {
        return [x, y];
    } else {
        return [y, x];
    }
}

function deepClone(x) {
    return JSON.parse(JSON.stringify(x));
}

// Get random integer within [min, max).
function getRandomInt(min, max) {
    min = Math.ceil(min);
    max = Math.floor(max);
    return Math.floor(Math.random() * (max - min)) + min;
}

function childNodesOf() {
    return this.childNodes;
}

function linePointDistance(x1, y1, x2, y2, x0, y0) {
    var rx = x0 - x1;
    var ry = y0 - y1;
    var lx = x2 - x1;
    var ly = y2 - y1;
    var l = Math.sqrt(lx * lx + ly * ly);
    var proj = (rx * lx + ry * ly) / l;
    if (proj < l) {
        // projection is within segment
        return Math.abs(ly * x0 - lx * y0 + x2 * y1 - y2 * x1) / l;
    } else if (proj < 0) {
        // projection is to the left of segment
        return Math.sqrt(rx * rx + ry * ry);
    } else {
        // projection is to the right of segment
        return Math.sqrt(Math.pow(x0 - x2, 2) + Math.pow(y0 - y2, 2));
    }
}

//////////////////////////////////////////////////////////////////////////////
// Superline manipulation

var EMPTY_SUPERLINE = {
    phase: 0,
    summed: false,
    weight: 0,
};

// NOTE: must maintain invariant that terminals precede all other nodes.
// Also, the order of nodes is critical!  If you move the nodes around,
// make sure the lines are also reversed.

function availSuperlineLabels(diagram, count) {
    // avoid 0, which might get confused for j = 0
    var counter = 1;
    var labels = [];
    while (labels.length < count) {
        while (diagram.superlines.hasOwnProperty(counter.toString())) {
            counter += 1;
        }
        labels.push(counter.toString());
        counter += 1;
    }
    return labels;
}

function newLabel(label) {
    var match = /^([\s\S]*?)(\d*)$/.exec(label);
    return match[1] + (Number(match[2]) + 1).toString();
}

function changePhase(superline, phase) {
    superline.phase = mod(superline.phase + phase, 4);
}

function mergeSuperlines(superline1, superline2) {
    superline1 = Object.assign({}, superline1);
    changePhase(superline1, superline2.phase);
    superline1.weight += superline2.weight;
    superline1.summed |= superline2.summed;
    return superline1;
}

//////////////////////////////////////////////////////////////////////////////
// Line manipulation

function reverseLine(line) {
    var line = Object.assign({}, line);
    line.direction = -line.direction;
    line.arrowPos = 1.0 - line.arrowPos;
    return line;
}

function plainLine(superlineId) {
    return {
        superline: superlineId,
        direction: 0,
        arrowPos: 0.5,
    };
}

function canonicalizeLine(line) {
    return {
        line: Object.assign({}, line, {direction: line.direction % 2}),
        phase: mod(Math.trunc(line.direction / 2), 2) * 2,
    };
}

function joinLines(line1, line2) {
    var superlines = sortTwo(line1.superline, line2.superline);
    return Object.assign(canonicalizeLine({
        superline: superlines[0],
        direction: line1.direction + line2.direction,
        arrowPos: (line1.arrowPos + line2.arrowPos) / 2
    }), {otherSuperline: superlines[1]});
}

//////////////////////////////////////////////////////////////////////////////
// Node manipulation

function endNodeAndLineIndices(nodes, lineId) {
    var nodeAndLineIndices = [];
    if (lineId === undefined || lineId === null) {
        throw new Error("lineId must not be null");
    }
    nodes.forEach(function (node, nodeIndex) {
        node.lines.forEach(function (nodeLineId, lineIndex) {
            if (nodeLineId == lineId) {
                nodeAndLineIndices.push({
                    node: nodeIndex,
                    lineIndex: lineIndex
                });
            }
        })
    });
    if (nodeAndLineIndices.length != 2) {
        throw new Error("line must be connected at 2 points, "
                      + `not ${nodeAndLineIndices.length}`);
    }
    return nodeAndLineIndices;
}

function endNodeIndices(nodes, lineId) {
    return endNodeAndLineIndices(nodes, lineId).map(x => x.node);
}

function otherNodeAndLineIndex(nodes, selfIndex, lineIndex) {
    var lineId = nodes[selfIndex].lines[lineIndex];
    if (lineId === undefined) {
        throw new Error(`cannot find line with lineIndex = ${lineIndex}`);
    }
    var nodeAndLineIndices = endNodeAndLineIndices(nodes, lineId);
    if (nodeAndLineIndices[0].node == selfIndex &&
        nodeAndLineIndices[0].lineIndex == lineIndex) {
        return nodeAndLineIndices[1];
    } else {
        return nodeAndLineIndices[0];
    }
}

function otherNodeIndex(nodes, selfIndex, lineIndex) {
    return otherNodeAndLineIndex(nodes, selfIndex, lineIndex).node;
}

function isLeftOfLine(nodes, selfIndex, lineIndex) {
    var other = otherNodeAndLineIndex(nodes, selfIndex, lineIndex);
    if (other.node == selfIndex) {
        return other.lineIndex > selfIndex;
    } else {
        return other.node > selfIndex;
    }
}

function angleToOtherNode(nodes, selfIndex, lineIndex) {
    var self = nodes[selfIndex];
    var other = nodes[otherNodeIndex(nodes, selfIndex, lineIndex)];
    return Math.atan2(other.y - self.y, other.x - self.x);
}

function nearestNodeIndices(nodes, count, x, y) {
    var nodeIndices = nodes.map((node, nodeIndex) => ({
        distance: Math.pow(x - node.x, 2) + Math.pow(y - node.y, 2),
        index: nodeIndex
    }));
    nodeIndices.sort((x, y) => x.distance - y.distance);
    return nodeIndices.slice(0, count).map(node => node.index);
}

//////////////////////////////////////////////////////////////////////////////
// Diagram manipulation

var EMPTY_DIAGRAM = {
    nodes: [],
    superlines: {},
    lines: {}
};

function w3jDiagram(a, b, c, x, y) {
    if (a == b || b == c || c == a) {
        throw new Error("cannot create w3jDiagram with conflicting labels");
    }
    return {
        nodes: [
            {
                type: "terminal",
                lines: [a],
                x: x - 50,
                y: y + 50
            },
            {
                type: "terminal",
                lines: [b],
                x: x + 50,
                y: y + 50
            },
            {
                type: "terminal",
                lines: [c],
                x: x,
                y: y - 70
            },
            {
                type: "w3j",
                lines: [a, b, c],
                x: x,
                y: y
            },
        ],
        lines: {
            [a]: plainLine(a),
            [b]: plainLine(b),
            [c]: plainLine(c)
        },
        superlines: {
            [a]: EMPTY_SUPERLINE,
            [b]: EMPTY_SUPERLINE,
            [c]: EMPTY_SUPERLINE
        }
    };
}

function cgDiagram(a, b, c, x, y) {
    var diagram = w3jDiagram(a, b, c, x, y);
    diagram.lines[c] = Object.assign({}, diagram.lines[c], {
        direction: 1
    });
    diagram.superlines[b] = Object.assign({}, diagram.superlines[b], {
        phase: 2
    });
    diagram.superlines[c] = Object.assign({}, diagram.superlines[c], {
        weight: 1
    });
    return diagram;
}

function findNearestLineId(diagram, x, y) {
    var candidates = Object.keys(diagram.lines).map(function(lineId) {
        var nodeIndices = endNodeIndices(diagram.nodes, lineId);
        return {
            distance: linePointDistance(
                diagram.nodes[nodeIndices[0]].x,
                diagram.nodes[nodeIndices[0]].y,
                diagram.nodes[nodeIndices[1]].x,
                diagram.nodes[nodeIndices[1]].y,
                x, y
            ),
            id: lineId
        };
    });
    candidates.sort((x, y) => x.distance - y.distance);
    return candidates.slice(0, 1).map(x => x.id);
}

function mergeDiagrams(diagram1, diagram2) {
    var diagram = deepClone(diagram1);
    Object.keys(diagram2.superlines).forEach(function(superlineId) {
        var self = diagram2.superlines[superlineId];
        if (diagram.superlines.hasOwnProperty(superlineId)) {
            diagram.superlines[superlineId] =
                mergeSuperlines(diagram.superlines[superlineId], self);
        } else {
            diagram.superlines[superlineId] = self;
        }
    });
    var renames = {};
    Object.keys(diagram2.lines).forEach(function(lineId) {
        var newLineId = lineId;
        while (diagram.lines.hasOwnProperty(newLineId)) {
            // name collision
            newLineId = newLabel(newLineId);
        }
        renames[lineId] = newLineId;
        var line = diagram2.lines[lineId];
        diagram.lines[newLineId] = line;
    });
    var terminals = [];
    diagram2.nodes.forEach(function(node) {
        node.lines = node.lines.map(lineId => renames[lineId]);
        if (node.type == "terminal") {
            terminals.push(node);
        } else {
            diagram.nodes.push(node);
        }
    });
    diagram.nodes = terminals.concat(diagram.nodes);
    return diagram;
}

function joinTerminals(diagram, terminalIndex1, terminalIndex2) {
    if (diagram.nodes[terminalIndex1].type != "terminal" ||
        diagram.nodes[terminalIndex2].type != "terminal") {
        throw new Error("cannot join non-terminals");
    }
    var other1 = otherNodeIndex(diagram.nodes, terminalIndex1, 0);
    var other2 = otherNodeIndex(diagram.nodes, terminalIndex2, 0);
    if (other1 == other2) {
        // FIXME
        error("Cannot connect node to itself (not yet implemented)")
        return diagram;
    } else if (other1 > other2) {
        // we only handle cases where LEFT < RIGHT
        return joinTerminals(diagram, terminalIndex2, terminalIndex1);
    }
    var lineId1 = diagram.nodes[terminalIndex1].lines[0];
    var lineId2 = diagram.nodes[terminalIndex2].lines[0];
    if (lineId1 == lineId2) {
        // FIXME loops are not yet implemented
        error("Cannot join terminals sharing the same line (not yet implemented)");
        return diagram;
    }
    diagram = deepClone(diagram);

    // join with other node
    diagram.nodes[other2].lines[diagram.nodes[other2].lines.indexOf(lineId2)] =
        lineId1;

    // merge the lines (be careful with orientation)
    var line1 = diagram.lines[lineId1];
    var line2 = diagram.lines[lineId2];
    if (other1 > terminalIndex1) {
        line1 = reverseLine(line1);
    }
    if (terminalIndex2 > other2) {
        line2 = reverseLine(line2);
    }
    var joined = joinLines(line1, line2);
    var superlineId1 = joined.line.superline;
    var superlineId2 = joined.otherSuperline;
    changePhase(diagram.superlines[superlineId1], joined.phase);
console.log(joined.phase);
    diagram.lines[lineId1] = joined.line;
    delete diagram.lines[lineId2];

    // equate superlines and merge their factors
    diagram.superlines[superlineId1] =
        mergeSuperlines(diagram.superlines[superlineId1],
                        diagram.superlines[superlineId2]);
    delete diagram.superlines[superlineId2];

    // delete the terminal nodes
    var terminals = [terminalIndex1, terminalIndex2];
    terminals.sort((x, y) => y - x);
    terminals.forEach(terminalIndex => {
        if (diagram.nodes[terminalIndex].type != "terminal") {
            throw new Error("BUG: I am deleting the wrong node?");
        }
        diagram.nodes.splice(terminalIndex, 1);
    });
    return diagram;
}

function addW1j(diagram, lineId) {
    diagram = deepClone(diagram);
    var line = diagram.lines[lineId];
    // cycle through all possible directions
    if (line.direction > 0) {
        line.direction = -1;
    } else if (line.direction < 0) {
        line.direction = 0;
    } else {
        line.direction = 1;
    }
    return diagram;
}

function add2j(diagram, lineId) {
    diagram = deepClone(diagram);
    var superline = diagram.superlines[diagram.lines[lineId].superline];
    changePhase(superline, 2);
    return diagram;
}

function deleteNode(diagram, nodeIndex) {
    diagram = deepClone(diagram);
    var terminals = []
    var node = diagram.nodes[nodeIndex];
    if (node.type == "terminal") {
        var otherIndex = otherNodeIndex(diagram.nodes, nodeIndex, 0);
        if (diagram.nodes[otherIndex].type != "terminal") {
            throw new Error("cannot delete terminal of node");
        }
        var superlineId = diagram.lines[node.lines[0]].superline;
        delete diagram.lines[node.lines[0]];
        var danglingSuperline = true;
        Object.keys(diagram.lines).forEach(function(lineId) {
            if (diagram.lines[lineId].superline == superlineId) {
                danglingSuperline = false;
            }
        });
        if (danglingSuperline) {
            delete diagram.superlines[superlineId];
        }
        var terminals = [nodeIndex, otherIndex];
        terminals.sort((x, y) => y - x);
        terminals.forEach(terminalIndex => {
            diagram.nodes.splice(terminalIndex, 1);
        });
    } else {
        node.lines.forEach(function(lineId, lineIndex) {
            var otherIndex = otherNodeIndex(diagram.nodes, nodeIndex, lineIndex);
            if (otherIndex == nodeIndex) {
                delete diagram.lines[lineIndex];
                return;
            }
            if (otherIndex < nodeIndex) {
                diagram.lines[lineId] = reverseLine(diagram.lines[lineId]);
            }
            terminals.push({
                type: "terminal",
                lines: [lineId],
                x: node.x,
                y: node.y
            });
        });
        diagram.nodes.splice(nodeIndex, 1);
        diagram.nodes = terminals.concat(diagram.nodes);
    }
    return diagram;
}

//////////////////////////////////////////////////////////////////////////////
// Diagrammatic rules

function flipW3jRule(diagram, nodeIndex) {
    if (diagram.nodes[nodeIndex].type != "w3j") {
        return diagram;
    }
    diagram = deepClone(diagram);
    diagram.nodes[nodeIndex].lines.reverse();
    diagram.nodes[nodeIndex].lines.forEach(function(lineId) {
        var superlineId = diagram.lines[lineId].superline;
        diagram.superlines[superlineId] =
            mergeSuperlines(diagram.superlines[superlineId], {
                phase: 1,
                summed: false,
                weight: 0,
            });
    });
    return diagram;
}

function flipW1jRule(diagram, lineId) {
    if (diagram.lines[lineId].direction) {
        diagram = deepClone(diagram);
        diagram.lines[lineId].direction *= -1;
        var superlineId = diagram.lines[lineId].superline;
        diagram.superlines[superlineId] =
            mergeSuperlines(diagram.superlines[superlineId], {
                phase: 2,
                summed: false,
                weight: 0,
            });
    }
    return diagram;
}

function threeArrowRule(diagram, nodeIndex) {
    diagram = deepClone(diagram);
    var node = diagram.nodes[nodeIndex];
    if (node.type != "w3j") {
        return diagram;
    }
    var direction = 0;
    // figure out the direction that would minimize the phase change
    node.lines.forEach(function(lineId, lineIndex) {
        var line = diagram.lines[lineId];
        if (isLeftOfLine(diagram.nodes, nodeIndex, lineIndex)) {
            direction -= line.direction;
        } else {
            direction += line.direction;
        }
    });
    if (direction == 0) {
        // we still don't have a direction, so let's just pick "outgoing"
        direction = 1;
    } else if (direction == 3) {
        // if all outgoing, completely reverse the direction
        direction = -2;
    } else {
        // normalize to one
        direction = direction / Math.abs(direction);
    }
    node.lines.forEach(function(lineId, lineIndex) {
        var line = diagram.lines[lineId];
        if (isLeftOfLine(diagram.nodes, nodeIndex, lineIndex)) {
            line.direction += direction;
        } else {
            line.direction -= direction;
        }
        var canonicalized = canonicalizeLine(line);
        diagram.lines[lineId] = canonicalized.line;
        changePhase(diagram.superlines[line.superline], canonicalized.phase);
    });
    return diagram;
}

//////////////////////////////////////////////////////////////////////////////
// Drawing

function w3jOrientation(nodes, index) {
    var node = nodes[index];
    if (node.type != "w3j") {
        throw new Error("cannot get orientation of generic node");
    }
    var lines = [0, 1, 2].map(function(lineIndex) {
        return [lineIndex, angleToOtherNode(nodes, index, lineIndex)];
    });
    lines.sort(function (line1, line2) {
        return line1[1] - line2[1];
    });
    if (lines.map(function(line) {
        return mod(line[0] - lines[0][0], 3);
    }).join() === [0, 1, 2].join()) {
        return 1;
    } else {
        return -1;
    }
}

function twoJColor(lineId) {
    var diagram = current(hist);
    var superlineId = diagram.lines[lineId].superline;
    return mod(diagram.superlines[superlineId].phase, 4) >= 2 ?
           "#ac53b3" : "#051308";
}

function drawArrow(container, linesData, diagram) {
    const arrowHeadSize = 25;
    function data(i) {
        var line = linesData[i];
        if (line.direction == 0) {
            return [];
        }
        if (!(line.arrowPos >= 0.0 && line.arrowPos <= 1.0)) {
            throw new Error(`invalid arrowPos: ${line.arrowPos}`);
        }
        var nodeIndices = endNodeIndices(diagram.nodes, line.id);
        var x0 = diagram.nodes[nodeIndices[0]].x;
        var y0 = diagram.nodes[nodeIndices[0]].y;
        var x1 = diagram.nodes[nodeIndices[1]].x;
        var y1 = diagram.nodes[nodeIndices[1]].y;
        const lineLength = Math.sqrt(Math.pow(x1 - x0, 2)
                                   + Math.pow(y1 - y0, 2));
        // the position we want is the tip of the arrow (which follows the
        // contour of the line), but we want to try to keep the body of the
        // arrow centered
        const posCorrection = line.direction * arrowHeadSize / 2 / lineLength;
        const pos = line.arrowPos + posCorrection;
        var x = x0 + (x1 - x0) * pos;
        var y = y0 + (y1 - y0) * pos;
        var angle = Math.atan2(y1 - y0, x1 - x0) * 180 / Math.PI;
        if (line.direction < 0) {
            angle += 180;
        }
        return [{
            lineId: line.id,
            transform: `translate(${x}, ${y}),rotate(${angle})`
        }];
    };
    var selection = container.selectAll(childNodesOf)
                             .filter("use")
                             .data(data);
    selection.exit().remove();
    var use = selection.enter()
                       .append("use")
                       .attr("href", "#arrowhead")
                       .attr("x", -arrowHeadSize)
                       .attr("y", -arrowHeadSize / 2)
                       .attr("width", arrowHeadSize)
                       .attr("height", arrowHeadSize)
                       .on("click", function(d) {
                           var diagram = current(hist);
                           diagram = flipW1jRule(diagram, d.lineId);
                           saveDiagram(hist, diagram);
                           updateDiagram(diagram);
                       });
    selection.merge(use)
             .attr("fill", d => twoJColor(d.lineId))
             .attr("transform", d => d.transform);
}

function drawDiagramLines(diagram, container) {
    // HACK: indexes are used here because d3 captures the
    //       objects directly and doesn't change them :/
    var data = Object.keys(diagram.lines).map(function(id) {
        var line = diagram.lines[id];
        var nodeIndices = endNodeIndices(diagram.nodes, id);
        var x0 = diagram.nodes[nodeIndices[0]].x;
        var y0 = diagram.nodes[nodeIndices[0]].y;
        var x1 = diagram.nodes[nodeIndices[1]].x;
        var y1 = diagram.nodes[nodeIndices[1]].y;
        var textOffsetAngle = Math.atan2(y1 - y0, x1 - x0) + Math.PI / 2;
        var textOffset = 17.0;
        var d = `M ${x0} ${y0} ` +
                `L ${x1} ${y1}`;
        return Object.assign({
            id: id,
            x0: x0,
            y0: y0,
            x1: x1,
            y1: y1,
            textX: (x0 + x1) / 2 + textOffset * Math.cos(textOffsetAngle),
            textY: (y0 + y1) / 2 + textOffset * Math.sin(textOffsetAngle),
            d: d
        }, line);
    });
    var selection = container.selectAll("g.line")
                             .data(Array.from(data.keys()));
    selection.exit().remove();
    var g = selection.enter()
                     .append("g")
                     .attr("class", "line")
    var path = g.append("path")
                .attr("fill", "transparent")
                .attr("stroke", "black")
                .attr("stroke-width", "2");
    g.append("text")
     .attr("class", "line-label")
     .attr("alignment-baseline", "middle")
     .attr("text-anchor", "middle")
     .attr("font-size", "large");
    var arrowG = g.append("g");
    var merged = selection.merge(g).selectAll(childNodesOf);
    merged.filter("g").call(drawArrow, data, diagram);
    merged.filter("path")
          .attr("d", i => data[i].d)
          .attr("stroke", i => twoJColor(data[i].id));
    merged.filter("text")
          .attr("x", i => data[i].textX)
          .attr("y", i => data[i].textY)
          .attr("fill", i => twoJColor(data[i].id))
          .html(i => data[i].superline);
}

function drawDiagramNodes(diagram, container, hist) {
    var nodes = diagram.nodes;
    var dragStarted = false; // avoid unnecessary saves
    var drag = d3.drag()
                 .on("drag", function(i) {
                     if (controls.modifiers == 0) {
                         diagram = current(hist);
                         var node = diagram.nodes[i];
                         node.x = d3.event.x;
                         node.y = d3.event.y;
                         updateDiagramSuperficially(diagram);
                         dragStarted = true;
                     }
                 })
                 .on("end", function(i) {
                     if (dragStarted) {
                         saveDiagram(hist, diagram);
                         dragStarted = false;
                     }
                 });
    var data = nodes.map(function(node, index) {
        return Object.assign({
            index: index,
            orientation: node.type == "w3j" ?
                         w3jOrientation(nodes, index) : 0
        }, node);
    });
    // HACK: indices aren't stable!
    container.selectAll(childNodesOf).filter("g").remove();
    var selection = container.selectAll(childNodesOf)
                             .filter("g")
                             .data(Array.from(data.keys()));
    selection.exit().remove();
    var g = selection.enter()
                     .append("g")
                     .call(drag)
                     .on("click", function(i) {
                         var diagram = current(hist);
                         diagram = flipW3jRule(diagram, i);
                         if (controls.modifiers != SHIFT) { // do it twice!
                             diagram = flipW3jRule(diagram, i);
                         }
                         saveDiagram(hist, diagram);
                         updateDiagram(diagram);
                     })
                     .on("mousedown", function(i) {
                         if (d3.event.button == 1) {
                             var diagram = threeArrowRule(
                                 current(hist), i);
                             saveDiagram(hist, diagram);
                             updateDiagram(diagram);
                         }
                     });
    g.append("circle")
    g.filter(i => !["w3j", "terminal"].includes(data[i].type))
     .append("text")
     .attr("class", "node-label")
     .attr("alignment-baseline", "middle")
     .attr("text-anchor", "middle")
     .attr("font-size", "large")
    var circularArrowSize = 30;
    g.filter(i => data[i].type == "w3j")
     .append("use")
     .attr("href", "#clockwise")
     .attr("x", -circularArrowSize / 2)
     .attr("y", -circularArrowSize / 2)
     .attr("width", circularArrowSize)
     .attr("height", circularArrowSize)
     .attr("fill", "white");
    var merged = selection
        .merge(g)
        .attr("transform", i => `translate(${data[i].x}, ${data[i].y})`);
    merged.select("circle")
          .attr("r", function(i) {
              var node = data[i];
              if (node.type == "terminal") {
                  return 10;
              } else if (node.type == "w3j") {
                  return 18;
              } else {
                  return 22;
              }
          })
          .style("fill", function(i) {
              var node = data[i];
              if (node.type == "terminal") {
                  return "rgba(0, 0, 0, 0.2)";
              } else if (node.type == "w3j") {
                  if (node.orientation > 0) {
                      return "#d98b7c";
                  } else {
                      return "#2eb0ba";
                  }
              } else {
                  return "#ddd";
              }
          });
    merged.select("text")
          .attr("fill", function(i) {
              var node = data[i];
              if (node.type == "w3j") {
                  return "#fff";
              } else {
                  return "#000";
              }
          })
          .text(i => data[i].type);
    merged.select("use")
          .attr("transform", i => data[i].orientation > 0 ? "scale(-1,1)" : "");
}

function drawDragTrail(trail, container) {
}

function drawDiagram(diagram) {
    drawDiagramNodes(diagram, d3.select("#diagram-nodes"), hist);
    drawDiagramLines(diagram, d3.select("#diagram-lines"));
    drawDragTrail(controls.dragTrail, document.getElementById("diagram-drag-trail"));
}

//////////////////////////////////////////////////////////////////////////////
// Rendering text

function renderTableau(diagram) {
    document.getElementById("version").textContent = hist.version;
    document.getElementById("tableau-container").style.background = hist.version;
    var tableau = document.getElementById("tableau");
    tableau.getElementsByClassName("main")[0].remove();
    var main = document.createElement("tbody");
    main.className = "main";
    var superlineIds = Object.keys(diagram.superlines);
    superlineIds.sort((x, y) => {
        var d = x.length - y.length;
        if (d == 0) {
            d = Number(x > y) - Number(x < y);
        }
        return d;
    });
    superlineIds.forEach(function(superlineId) {
        var superline = diagram.superlines[superlineId];
        var tr = document.createElement("tr");
        // summed
        var td = document.createElement("td");
        td.className = "summed";
        td.textContent = superline.summed ? "∑" : "";
        tr.appendChild(td);
        // name
        var td = document.createElement("td");
        td.className = "name";
        td.textContent = superlineId;
        tr.appendChild(td);
        // phase
        td = document.createElement("td");
        td.className = "phase";
        switch (mod(superline.phase, 4)) {
            case 0:
                td.innerHTML = '  ';
                break;
            case 1:
                td.innerHTML = ' .';
                break;
            case 2:
                td.innerHTML = ': ';
                break;
            case 3:
                td.innerHTML = ':.';
                break;
        }
        tr.appendChild(td);
        // weight
        td = document.createElement("td");
        td.className = "weight";
        if (superline.weight) {
            td.textContent = `${superline.weight > 0 ? "+" : ""}${superline.weight}`;
        }
        tr.appendChild(td);
        main.appendChild(tr);
    });
    tableau.appendChild(main);
}

function renderNodeLine(diagram, nodeIndex, lineIndex, summedVars) {
    var lineId = diagram.nodes[nodeIndex].lines[lineIndex];
    var mNaked = `m_{${lineId}}`;
    var jm = {
        j: `j_{${diagram.lines[lineId].superline}}`,
        m: mNaked
    };
    var line = diagram.lines[lineId];
    var otherIndex = otherNodeIndex(diagram.nodes, nodeIndex, lineIndex);
    if (otherIndex < nodeIndex && line.direction != 0) {
        jm.m = `\\overline{${jm.m}}`;
    }
    var summedJ = line.superline.summed;
    if (summedJ) {
        summedVars.js[jm.j] = true;
    }
    var summedM = diagram.nodes[otherIndex].type != "terminal";
    if (summedM) {
        summedVars.ms[mNaked] = true;
    }
    return jm;
}

function renderEquation(diagram, container) {
    var s = "";
    var summedVars = {js: {}, ms: {}};
    var phases = [];
    diagram.nodes.forEach(function(node, nodeIndex) {
        if (node.type == "terminal") {
        } else if (node.type == "w3j") {
            s += "\\begin{pmatrix}";
            var jRow = "";
            var mRow = "";
            node.lines.forEach(function(lineId, lineIndex) {
                if (lineIndex > 0) {
                    jRow += " & ";
                    mRow += " & ";
                }
                var jm = renderNodeLine(diagram, nodeIndex,
                                        lineIndex, summedVars);
                jRow += jm.j;
                mRow += jm.m;
            });
            s += jRow + " \\\\";
            s += mRow + " \\\\";
            s += "\\end{pmatrix}";
        } else {
            s += "\\mathtt{${node.type}}_{";
            node.lines.forEach(function(lineId, lineIndex) {
                if (lineIndex > 0) {
                    s += " ";
                }
                var jm = renderNodeLine(diagram, nodeIndex,
                                        lineId, summedVars);
                s += jm[0] + " " + jm[1];
            });
            s += "}";
        }
    });
    Object.keys(diagram.lines).forEach(function(lineId) {
        var line = diagram.lines[lineId];
        if (line.direction != 0) {
            phases.push(`+ j_{${diagram.lines[lineId].superline}} `
                      + `${line.direction < 0 ? "+" : "-"} m_{${lineId}}`);
        }
    });
    var weights = "";
    Object.keys(diagram.superlines).forEach(function(superlineId) {
        var superline = diagram.superlines[superlineId];
        switch (mod(superline.phase, 4)) {
            case 0:
                break;
            case 1:
                phases.push(`+ j_{${superlineId}}`);
                break;
            case 2:
                phases.push(`+ 2 j_{${superlineId}}`);
                break;
            case 3:
                phases.push(`- j_{${superlineId}}`);
                break;
        }
        if (superline.weight) {
            weights += ` (2 j_{${superlineId}} + 1)^{${superline.weight} / 2}`;
        }
    });
    var summedVarsStr = Array.from(Object.keys(summedVars.js)).join(" ")
                      + " "
                      + Array.from(Object.keys(summedVars.ms)).join(" ");
    if (summedVarsStr != " ") {
        summedVarsStr = `\\sum_{${summedVars}}`;
    }
    var phasesStr = phases.join(" ");
    if (phasesStr) {
        if (phasesStr.startsWith("+ ")) {
            phasesStr = phasesStr.substr(2);
        }
        phasesStr = `(-1)^{${phases}}`;
    }
    container.textContent = `\\[${summedVarsStr} ${weights} ${phasesStr} ${s}\\]`;
    MathJax.Hub.Queue(["Typeset", MathJax.Hub, container]);
}

function updateDiagramSuperficially(diagram) {
    drawDiagram(diagram);
    renderTableau(diagram);
}

function updateDiagram(diagram) {
    updateDiagramSuperficially(diagram);
    document.getElementById("equation-container")
            .className = "out-of-date";
}

//////////////////////////////////////////////////////////////////////////////
// History tracking

var hist = {
    // each time the diagram undergoes a non-rule change the version is regenerated
    version: generateVersion(),
    history: [],
    undoDepth: 0
};

function generateVersion() {
    var s = "#";
    var x;
    for (var i = 0; i < 3; ++i) {
        x = getRandomInt(192, 256).toString(16);
        if (x.length == 1) {
            x = "0" + x;
        }
        s += x;
    }
    return s;
}

function setHash(diagram) {
    currentHash = "#" + encodeURIComponent(JSON.stringify(diagram))
    window.location.hash = currentHash;
}

function saveDiagramWith(hist, diagram, bump) {
    setHash(diagram);
    hist.history.splice(hist.history.length - hist.undoDepth, hist.undoDepth);
    if (bump) {
        hist.version = generateVersion();
    }
    hist.history.push({
        version: hist.version,
        diagram: deepClone(diagram)
    });
    hist.undoDepth = 0;
}

function saveNewDiagram(hist, diagram) {
    return saveDiagramWith(hist, diagram, true);
}

function saveDiagram(hist, diagram) {
    return saveDiagramWith(hist, diagram, false);
}

function updateCurrent(hist, changed) {
    var entry = hist.history[hist.history.length - 1 - hist.undoDepth];
    hist.version = entry.version;
    if (changed) {
        setHash(entry.diagram);
    }
    return entry.diagram;
}

function current(hist) {
    return updateCurrent(hist, false);
}

function undo(hist) {
    var changed = false;
    if (hist.undoDepth < hist.history.length - 1) {
        hist.undoDepth += 1;
        changed = true;
    }
    return updateCurrent(hist, changed);
}

function redo(hist) {
    var changed = false;
    if (hist.undoDepth > 0) {
        hist.undoDepth -= 1;
        changed = true;
    }
    return updateCurrent(hist, changed);
}

//////////////////////////////////////////////////////////////////////////////
// Controls

var controls = {
    modifiers: 0,
    mouseX: null,
    mouseY: null,
    dragTrail: []
};

const ALT = 0x1;
const CTRL = 0x2;
const SHIFT = 0x4;

function updateKeyState(controls, event) {
    controls.modifiers = event.altKey
                       | (event.ctrlKey << 1)
                       | (event.shiftKey << 2);
}

window.addEventListener("keydown", function(event) {
    updateKeyState(controls, event);
    if (controls.modifiers == CTRL && event.key == "z") {
        updateDiagram(undo(hist));
    }
    if (controls.modifiers == CTRL && event.key == "y") {
        updateDiagram(redo(hist));
    }

    // reload
    if (controls.modifiers == 0 && event.key == "r") {
        window.location.href = "";
    }

    // mouse events require the position
    if (controls.mouseX === null) {
        error("Need to move the mouse before doing anything :/");
        return;
    }

    // create Clebsch–Gordan coefficient
    if (controls.modifiers == 0 && event.key == "c") {
        let diagram = current(hist);
        let labels = availSuperlineLabels(diagram, 3);
        let subdiagram = cgDiagram(labels[0],
                                   labels[1],
                                   labels[2],
                                   controls.mouseX,
                                   controls.mouseY);
        diagram = mergeDiagrams(diagram, subdiagram);
        saveNewDiagram(hist, diagram);
        updateDiagram(diagram);
    }

    // create Wigner 3-jm
    if (controls.modifiers == 0 && event.key == "w") {
        let diagram = current(hist);
        let labels = availSuperlineLabels(diagram, 3);
        let subdiagram = w3jDiagram(labels[0],
                                    labels[1],
                                    labels[2],
                                    controls.mouseX,
                                    controls.mouseY);
        diagram = mergeDiagrams(diagram, subdiagram);
        saveNewDiagram(hist, diagram);
        updateDiagram(diagram);
    }

    // attach
    if (controls.modifiers == 0 && event.key == "a") {
        let diagram = current(hist);
        let nearest = nearestNodeIndices(diagram.nodes, 2,
                                         controls.mouseX,
                                         controls.mouseY);
        if (!(nearest.length == 2 &&
              diagram.nodes[nearest[0]].type == "terminal" &&
              diagram.nodes[nearest[1]].type == "terminal")) {
            error("no nearby terminals found");
        } else {
            diagram = joinTerminals(diagram, nearest[0], nearest[1]);
            saveNewDiagram(hist, diagram);
            updateDiagram(diagram);
        }
    }

    // create Wigner 1-jm
    if (controls.modifiers == 0 && event.key == "m") {
        let diagram = current(hist);
        let nearest = findNearestLineId(diagram, controls.mouseX, controls.mouseY);
        if (nearest.length != 1) {
            error("no nearby line found");
        } else {
            diagram = addW1j(diagram, nearest);
            saveNewDiagram(hist, diagram);
            updateDiagram(diagram);
        }
    }

    // add 2j phase
    if (controls.modifiers == 0 && event.key == "j") {
        let diagram = current(hist);
        let nearest = findNearestLineId(diagram, controls.mouseX, controls.mouseY);
        if (nearest.length != 1) {
            error("no nearby line found");
        } else {
            diagram = add2j(diagram, nearest);
            saveNewDiagram(hist, diagram);
            updateDiagram(diagram);
        }
    }

    // delete node
    if (controls.modifiers == 0 && event.key == "x") {
        let diagram = current(hist);
        let nearest = nearestNodeIndices(diagram.nodes, 1,
                                         controls.mouseX,
                                         controls.mouseY);
        if (nearest.length != 1 ||
            (diagram.nodes[nearest[0]].type == "terminal" &&
             diagram.nodes[otherNodeIndex(diagram.nodes, nearest[0], 0)].type
                != "terminal")) {
            error("no nearby nodes found");
        } else {
            diagram = deleteNode(diagram, nearest[0]);
            saveNewDiagram(hist, diagram);
            updateDiagram(diagram);
        }
    }
});

//////////////////////////////////////////////////////////////////////////////
// Global stuff

var errorTimeout = 0;
function error(msg) {
    var notice = document.getElementById("notice");
    notice.className = "warning";
    notice.textContent = msg;
    if (errorTimeout) {
        window.clearTimeout(errorTimeout);
    }
    errorTimeout = window.setTimeout(function() {
        notice.textContent = " ";
    }, 10000);
}

function initializeDiagram() {
    var initialDiagram = EMPTY_DIAGRAM;
    if (window.location.hash.length >= 3) {
        initialDiagram =
            JSON.parse(decodeURIComponent(window.location.hash.substr(1)));
    }
    saveDiagram(hist, initialDiagram);
    updateDiagram(initialDiagram);
}

var currentHash = "";
window.addEventListener("hashchange", function() {
    // prevent this from observing our own changes
    if (currentHash != window.location.hash) {
        initializeDiagram();
        currentHash = window.location.hash;
    }
});

window.addEventListener("keyup", function(event) {
    updateKeyState(controls, event);
});

document.getElementById("diagram").addEventListener(
    "mousemove", function(event) {
        controls.mouseX = event.offsetX;
        controls.mouseY = event.offsetY;
    }
);
document.getElementById("equation").addEventListener("click", function() {
    renderEquation(current(hist), document.getElementById("equation"));
    document.getElementById("equation-container")
            .className = "";
});

initializeDiagram();