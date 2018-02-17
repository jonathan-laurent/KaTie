CSV=phos.csv unphos.csv snapshots.csv
QUERY_EXE=../../query
QUERY=query

all: $(CSV)

$(CSV) : t.json
	$(QUERY_EXE) -t $< -q $(QUERY) --native-snapshots --snapshots-names "snapshot.%.ka"

t.json: catphos.ka
	KaSim --no-log catphos.ka -trace $@

clean:
	rm -f inputs* $(CSV) *.json snap*