## Before recording
clear
mkdir ../temporary-path
ln -s /home/mzabani/Projects/codd/dist-newstyle/build/x86_64-linux/ghc-8.8.3/codd-0.1.0.0/x/codd-exe/build/codd-exe/codd-exe /home/mzabani/Projects/temporary-path/codd
export PATH="/home/mzabani/Projects/temporary-path:$PATH"




## To start recording
asciinema rec ../codd-add-migration.cast
# Let's add a new column to our table of employees
git status
git checkout -b add-num-children-column
nano add-num-children-column.sql

-- codd: non-destructive
ALTER TABLE employee ADD COLUMN num_children INT NOT NULL DEFAULT 0;

# Ok, now the SQL migration needs to be added and applied in this branch
codd add add-num-children-column.sql

# The migration has been timestamped and moved to a pre-configured directory
# And the on-disk hashes now reflect the change too
git status