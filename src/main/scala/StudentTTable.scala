import model.Student

object StudentTTable extends App {

	def studentTa(alpha: Double, df: Int): Double = {
		val t = Student.ta(alpha, df)
		t
	}

	// Заголовок таблицы
	val header = "n/α0\t0.95\t0.99\t0.999\n"
	val lineSeparator = "--------------------------------------\n"

	// Создаем StringBuilder для построения таблицы
	val tableBuilder = new StringBuilder()

	// Добавляем заголовок и разделитель
	tableBuilder.append(header)
	tableBuilder.append(lineSeparator)

	// Заполняем таблицу значениями
	for (df <- 1 to 10) {
		tableBuilder.append(s"$df\t\t")
		tableBuilder.append(f"${studentTa(0.95, df)}%.4f\t")
		tableBuilder.append(f"${studentTa(0.99, df)}%.4f\t")
		tableBuilder.append(f"${studentTa(0.999, df)}%.4f\n")
	}

	// Выводим таблицу
	println(tableBuilder.toString())
}
