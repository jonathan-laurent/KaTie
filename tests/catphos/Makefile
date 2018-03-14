CSV=phos.csv unphos.csv snapshots.csv
QUERY_EXE=../../query
QUERY=query

all: t.json
	$(QUERY_EXE) -t $< -q $(QUERY) \
	--native-snapshots --snapshots-names "snapshot.%.ka" \
	--output-directory out \
	--debug

t.json: catphos.ka
	KaSim --no-log catphos.ka -trace $@

clean:
	rm -rf inputs* $(CSV) *.json snap* out