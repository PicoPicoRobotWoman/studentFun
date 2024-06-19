package model

import scala.annotation.tailrec

object Student {


	def ta(a0: Double, n: Int): Double = {

		def alphaParallel(fun:(Double, Int) => Double, delta: Double): (Double, Int) => Double = {
			(t: Double, n: Int) => {
				fun(t, n) - delta
			}
		}

		def alpfaConst(fun: (Double, Int) => Double, n: Int): Double => Double = {
			t: Double => fun(t, n)
		}

		def findInterval(fun: Double => Double): (Double, Double) = {

			@tailrec
			def finder(fun: Double => Double, min: Double, max: Double): (Double, Double) = {

				fun(min)*fun(max) match {
					case value if value < 0 => (min, max)
					case value if value > 0 => finder(fun, min + 1, max + 1)
				}


			}

			finder(fun, 0, 1)

		}

		def bisection(tol: Double = 1e-10): (Double => Double, (Double, Double)) => Double = {

			(f: Double => Double, interval: (Double, Double)) => {

				@tailrec
				def loop(interval: (Double, Double)): Double = {
					val (a, b) = interval
					val midpoint = (a + b) / 2
					val fMid = f(midpoint)

					if (fMid == 0 || (b - a) / 2 < tol) midpoint
					else if (f(a) * fMid < 0) loop(a, midpoint)
					else loop(midpoint, b)
				}

				loop(interval)

			}

		}

		val parAlpha = alphaParallel(alpfa, a0)
		val conAlpha = alpfaConst(parAlpha, n)

		val interval = findInterval(conAlpha)

		bisection()(conAlpha, interval)

	}

	private def alpfa(ta: Double, n: Int): Double = {
		1 - ACM395(ta, n)
	}

	//ACM 395
	def ACM395(t: Double, n: Int): Double = {

		var a = 0.0
		var b = 0.0
		var y = 0.0

		val tSq = t * t
		y = tSq / n
		b = y + 1.0
		if (y > 1.0E-6) y = Math.log(b)
		a = n - 0.5
		b = 48.0 * a * a
		y = a * y

		y = (((((-0.4 * y - 3.3) * y - 24.0) * y - 85.5) /
			(0.8 * y * y + 100.0 + b) + y + 3.0) / b + 1.0) *
			Math.sqrt(y)
		2.0 * ACM209(-y)
	}

	//ACM 209
	private def ACM209(z: Double): Double = {
		var y: Double = 0.0
		var p: Double = 0.0
		var w: Double = 0.0

		if (z == 0.0)
			p = 0.0
		else {
			y = Math.abs(z) / 2
			if (y >= 3.0) {
				p = 1.0
			} else if (y < 1.0) {
				w = y * y
				p = ((((((((0.000124818987 * w
					- 0.001075204047) * w + 0.005198775019) * w
					- 0.019198292004) * w + 0.059054035642) * w
					- 0.151968751364) * w + 0.319152932694) * w
					- 0.5319230073) * w + 0.797884560593) * y * 2.0
			} else {
				y = y - 2.0
				p = (((((((((((((-0.000045255659 * y
					+ 0.00015252929) * y - 0.000019538132) * y
					- 0.000676904986) * y + 0.001390604284) * y
					- 0.00079462082) * y - 0.002034254874) * y
					+ 0.006549791214) * y - 0.010557625006) * y
					+ 0.011630447319) * y - 0.009279453341) * y
					+ 0.005353579108) * y - 0.002141268741) * y
					+ 0.000535310849) * y + 0.999936657524
			}
		}

		if (z > 0.0)
			(p + 1.0) / 2
		else
			(1.0 - p) / 2
	}


}
