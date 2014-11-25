
data=[]

for (i in xdata) data.push({
    "x": xdata[i],
    "y": ydata[i]
});

data2=[]
for (i in xdata) data2.push({
    "x": xdata[i],
    "y": equation(xdata[i])
})

d3.select("#matchgraph").html("Matched Graph <br/>"+equation)

console.log(data)
console.log(data2)

makeGraph(500,400,"#goalGraph", data)
makeGraph(500,400,"#bestGraph", data2)

//make reusable for components
function makeGraph(w, h, canvas, data) {

    //based on mike bostock's simple example 
    margin = 30
    width = w - 2 * margin,
    height = h - 2 * margin,

    x = d3.scale.linear().range([0, width]),
    y = d3.scale.linear().range([height, 0]);

    //ticksize
    var xAxis = d3.svg.axis()
        .scale(x)
        .orient("bottom");

    var yAxis = d3.svg.axis()
        .scale(y)
        .orient("left");

    var line = d3.svg.line()
        .x(function(d) {
            return x(d.x);
        })
        .y(function(d) {
            return y(d.y);
        })
        .interpolate("basis");

    x.domain(d3.extent(data, function(d) {
        return d.x;
    }));
    y.domain(d3.extent(data, function(d) {
        return d.y;
    }));

    var svg = d3.select(canvas).append("svg")
        .attr("width", width + 2 * margin)
        .attr("height", height + 2 * margin)
        .append("g")
        .attr("transform", "translate(" + margin + "," + margin + ")");

    svg.append("g")
        .attr("class", "xaxis")
        .attr("transform", "translate(0," + height + ")")
        .call(xAxis);

    svg.append("g")
        .attr("class", "yaxis")
        .call(yAxis)
        .append("text")
        .attr("transform", "rotate(-90)")
        .attr("y", 6)
        .attr("dy", ".5em")
        .style("text-anchor", "end")
        .text("Random point");

    svg.append("path")
        .attr("d", line(data))
        .attr("stroke", "steelblue")
        .attr("stroke-width", 2)
        .attr("fill", "none");

    //function sigmoid(t) { with (Math) { return 1 / (1 + exp(-2*t)); } }
    //chart.cartesian(sigmoid, [-5, 5, 100]);
}
