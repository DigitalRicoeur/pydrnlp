const words =
      [{word:"apples",count:6},
       {word:"peaches",count:3},
       {word:"pears",count:3},
       {word:"plums",count:1},
       {word:"birthday",count:1},
       {word:"tell",count:1}];

function maxCount(words) {
    return words.reduce(function (soFar, w) {
	return Math.max(soFar,w.count);
    },
			1);
};

function wordsToRelative(words) {
    const denom = maxCount(words);
    return words.map(function (w) {
	return {text:w.word, size: 100 * (w.count / denom)};
    });
};

const relativeWords = wordsToRelative(words);

const layout = d3.layout.cloud();

layout.size([500, 500]);
layout.words(relativeWords);
layout.fontSize(function(d) { return d.size; });
layout.on("end", draw);
layout.start();

//var fill = d3.scale.category20();

function draw(words) {
  d3.select("body").append("svg")
      .attr("width", layout.size()[0])
      .attr("height", layout.size()[1])
    .append("g")
      .attr("transform", "translate(" + layout.size()[0] / 2 + "," + layout.size()[1] / 2 + ")")
    .selectAll("text")
      .data(words)
    .enter().append("text")
      .style("font-size", function(d) { return d.size + "px"; })
      .style("font-family", "Impact")
      //.style("fill", function(d, i) { return fill(i); })
      .attr("text-anchor", "middle")
      .attr("transform", function(d) {
        return "translate(" + [d.x, d.y] + ")rotate(" + d.rotate + ")";
      })
      .text(function(d) { return d.text; });
}

/*
function totalCount(words) {
    return words.reduce(function (soFar, w) {
	return soFar + w.count;
    },
			0);
};

function wordsToPercent(words) {
    const denom = totalCount(words);
    return words.map(function (w) {
	return {text:w.word, size: 100 * (w.count / denom)};
    });
};

const percentWords = wordsToPercent(words);
*/
