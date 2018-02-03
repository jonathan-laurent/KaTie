CSV=phos.csv unphos.csv
QUERY_EXE=../../query
QUERY=query

all: $(CSV)

$(CSV) : t.json
	$(QUERY_EXE) -t $< -q $(QUERY)

t.json: catphos.ka
	KaSim --no-log catphos.ka -trace $@

clean:
	rm -f inputs* $(CSV) t.json