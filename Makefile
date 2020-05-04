build:
	mvn --batch-mode verify

	ls docs/figures/*.dot |xargs -n 1 sed -i \
	-e 's/\\\\U00000009/\\\\t/' \
	-e 's/\\\\U0000000a/\\\\n/' \
	-e 's/\\\\U0000000d/\\\\r/' \
	-e 's/\\\\U00000022/\\"/' \
	-e 's/\\\\U0000005c/\\\\/' \
	-e 's/\\\\U00000000-\\\\U0010ffff/Any/' \
	-e 's/\\\\U00000000/∞/' \
	-e 's/\\\\U0010ffff/∞/' \
	-e 's/\\\\U000000/0x/g'

	ls docs/figures/*.dot |xargs -n 1 dot -Tpng -O
