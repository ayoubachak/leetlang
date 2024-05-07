CC=gcc
CFLAGS=-I.
OBJDIR=temp


# Define all your .o files
OBJS = $(OBJDIR)/main.o $(OBJDIR)/chunk.o $(OBJDIR)/memory.o $(OBJDIR)/debug.o $(OBJDIR)/value.o $(OBJDIR)/table.o $(OBJDIR)/vm.o $(OBJDIR)/compiler.o $(OBJDIR)/scanner.o $(OBJDIR)/object.o 

# Define your target executable
main: $(OBJS)
	$(CC) -o $@ $^ $(CFLAGS)  # Ensure this line begins with a tab

$(OBJDIR)/%.o: %.c
	@mkdir -p $(OBJDIR)
	$(CC) -c -o $@ $< $(CFLAGS)  # Ensure this line begins with a tab

clean:
	rm -f $(OBJS)
	rmdir $(OBJDIR)  # Ensure this line begins with a tab
