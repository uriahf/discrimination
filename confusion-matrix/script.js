const langs = [
	"PT",
	"NT",
	"PF",
	"NF"
];
// hello world in 92 Languages

let charSize = 18;
let fallRate = charSize / 2;
let streams;
let baseColor = 180; // green in HSB
let baseSaturation = 100;
let baseBrightness = 100;
let minSpeed = 0.5;
let MaxSpeed = 2;


// -------------------------------
class Char {
	constructor(value, x, y, speed) {
		this.value = value;
		this.x = x;
		this.y = y;
		this.speed = speed;
	}

	draw() {
		const flick = random(100);
		// 10 percent chance of flickering a number instead
		if (flick < 0.1) {
			fill(baseColor, baseSaturation * 0.3, baseBrightness);
			text(round(random(9)), this.x, this.y);
		} else {
			text(this.value, this.x, this.y);
		}

		// fall down
		this.y = this.y > height ? 0 : this.y + this.speed;
	}
}

// -------------------------------------
class Stream {
	constructor(text, x) {
		const y = random(text.length * 4);
		const speed = random(minSpeed, MaxSpeed);
		this.chars = [];

		for (let i = text.length; i >= 0; i--) {
			this.chars.push(
				new Char(text[i], x, (y + text.length - i) * charSize, speed)
			);
		}
	}

	draw() {
		fill(baseColor, baseSaturation, baseBrightness);
		this.chars.forEach((c, i) => {
			// 30 percent chance of lit tail
			const lit = random(100);
			if (lit < 30) {
				if (i === this.chars.length - 1) {
					fill(baseColor, baseSaturation * 0.3, baseBrightness);
				} else {
					fill(baseColor, baseSaturation, baseBrightness * 0.9);
				}
			}

			c.draw();
		});
	}
}

function createStreams() {
	// create random streams from langs that span the width
	for (let i = 0; i < width; i += charSize) {
		streams.push(new Stream(random(langs), i));
	}
}

function reset() {
	streams = [];
	createStreams();
}

function setup() {
	createCanvas(innerWidth, innerHeight);
	reset();
	frameRate(60);
	colorMode(HSB);
	noStroke();
	textSize(charSize);
	textFont("monospace");
	background(0);
}

function draw() {
	background(0, 0.4);
	streams.forEach((s) => s.draw());
}

function windowResized() {
	resizeCanvas(innerWidth, innerHeight);
	background(0);
	reset();
}
