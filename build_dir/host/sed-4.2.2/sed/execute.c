/*  GNU SED, a batch stream editor.
    Copyright (C) 1989,90,91,92,93,94,95,98,99,2002,2003,2004,2005,2006,2008,2009
    Free Software Foundation, Inc.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 3, or (at your option)
    any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. */

#undef EXPERIMENTAL_DASH_N_OPTIMIZATION	/*don't use -- is very buggy*/
#define INITIAL_BUFFER_SIZE	50
#define FREAD_BUFFER_SIZE	8192

#include "sed.h"

#include <stddef.h>
#include <stdio.h>
#include <ctype.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "stat-macros.h"

#include <selinux/selinux.h>
#include <selinux/context.h>
#include "acl.h"

#ifdef __GNUC__
# if __GNUC__ > 2 || (__GNUC__ == 2 && __GNUC_MINOR__-0 >= 7)
   /* silence warning about unused parameter even for "gcc -W -Wunused" */
#  define UNUSED	__attribute__((unused))
# endif
#endif
#ifndef UNUSED
# define UNUSED
#endif


/* Sed operates a line at a time. */
struct line {
  char *text;		/* Pointer to line allocated by malloc. */
  char *active;		/* Pointer to non-consumed part of text. */
  size_t length;	/* Length of text (or active, if used). */
  size_t alloc;		/* Allocated space for active. */
  bool chomped;		/* Was a trailing newline dropped? */
  mbstate_t mbstate;
};

#define SIZEOF_LINE	offsetof (struct line, mbstate)

/* A queue of text to write out at the end of a cycle
   (filled by the "a", "r" and "R" commands.) */
struct append_queue {
  const char *fname;
  char *text;
  size_t textlen;
  struct append_queue *next;
  bool free;
};

/* State information for the input stream. */
struct input {
  /* The list of yet-to-be-opened files.  It is invalid for file_list
     to be NULL.  When *file_list is NULL we are currently processing
     the last file.  */

  char **file_list;

  /* Count of files we failed to open. */
  countT bad_count;	    

  /* Current input line number (over all files).  */
  countT line_number;	    

  /* True if we'll reset line numbers and addresses before
     starting to process the next (possibly the first) file.  */
  bool reset_at_next_file;

  /* Function to read one line.  If FP is NULL, read_fn better not
     be one which uses fp; in particular, read_always_fail() is
     recommended. */
  bool (*read_fn) (struct input *);	/* read one line */

  char *out_file_name;

  const char *in_file_name;

  /* Owner and mode to be set just before closing the file.  */
  struct stat st;

  /* if NULL, none of the following are valid */
  FILE *fp;

  bool no_buffering;
};


/* Have we done any replacements lately?  This is used by the `t' command. */
static bool replaced = false;

/* The current output file (stdout if -i is not being used. */
static struct output output_file;

/* The `current' input line. */
static struct line line;

/* An input line used to accumulate the result of the s and e commands. */
static struct line s_accum;

/* An input line that's been stored by later use by the program */
static struct line hold;

/* The buffered input look-ahead.  The only field that should be
   used outside of read_mem_line() or line_init() is buffer.length. */
static struct line buffer;

static struct append_queue *append_head = NULL;
static struct append_queue *append_tail = NULL;


/* increase a struct line's length, making some attempt at
   keeping realloc() calls under control by padding for future growth.  */
static void resize_line (struct line *, size_t);
static void
resize_line(lb, len)
  struct line *lb;
  size_t len;
{
  int inactive;
  inactive = lb->active - lb->text;

  /* If the inactive part has got to more than two thirds of the buffer,
   * remove it. */
  if (inactive > lb->alloc * 2)
    {
      memmove(lb->text, lb->active, lb->length);
      lb->alloc += lb->active - lb->text;
      lb->active = lb->text;
      inactive = 0;

      if (lb->alloc > len)
	return;
    }

  lb->alloc *= 2;
  if (lb->alloc < len)
    lb->alloc = len;
  if (lb->alloc < INITIAL_BUFFER_SIZE)
    lb->alloc = INITIAL_BUFFER_SIZE;
    
  lb->text = REALLOC(lb->text, inactive + lb->alloc, char);
  lb->active = lb->text + inactive;
}

/* Append `length' bytes from `string' to the line `to'. */
static void str_append (struct line *, const char *, size_t);
static void
str_append(to, string, length)
  struct line *to;
  const char *string;
  size_t length;
{
  size_t new_length = to->length + length;

  if (to->alloc < new_length)
    resize_line(to, new_length);
  memcpy(to->active + to->length, string, length);
  to->length = new_length;

  if (mb_cur_max > 1 && !is_utf8)
    while (length)
      {
        size_t n = MBRLEN (string, length, &to->mbstate);

        /* An invalid or imcomplete sequence is treated like a singlebyte character. */
        if (n == (size_t) -1 || n == (size_t) -2)
	  {
	    memset (&to->mbstate, 0, sizeof (to->mbstate));
	    n = 1;
	  }

        if (n > 0)
	  {
	    string += n;
	    length -= n;
	  }
        else
	  break;
      }
}

static void
str_append_modified(struct line *to, const char *string, size_t length,
		    enum replacement_types type)
{
  mbstate_t from_stat;

  if (type == REPL_ASIS)
    {
      str_append(to, string, length);
      return;
    }

  if (to->alloc - to->length < length * mb_cur_max)
    resize_line(to, to->length + length * mb_cur_max);

  memcpy (&from_stat, &to->mbstate, sizeof(mbstate_t));
  while (length)
    {
      wchar_t wc;
      int n = MBRTOWC (&wc, string, length, &from_stat);

      /* An invalid sequence is treated like a singlebyte character. */
      if (n == -1)
        {
          memset (&to->mbstate, 0, sizeof (from_stat));
          n = 1;
        }

      if (n > 0)
        string += n, length -= n;
      else
	{
	  /* Incomplete sequence, copy it manually.  */
	  str_append(to, string, length);
	  return;
	}

      /* Convert the first character specially... */
      if (type & (REPL_UPPERCASE_FIRST | REPL_LOWERCASE_FIRST))
	{
          if (type & REPL_UPPERCASE_FIRST)
            wc = towupper(wc);
          else
            wc = towlower(wc);

          type &= ~(REPL_LOWERCASE_FIRST | REPL_UPPERCASE_FIRST);
	  if (type == REPL_ASIS)
	    {
	      n = WCRTOMB (to->active + to->length, wc, &to->mbstate);
	      to->length += n;
	      str_append(to, string, length);
	      return;
	    }
        }

      else if (type & REPL_UPPERCASE)
        wc = towupper(wc);
      else
        wc = towlower(wc);

      /* Copy the new wide character to the end of the string. */
      n = WCRTOMB (to->active + to->length, wc, &to->mbstate);
      to->length += n;
      if (n == -1 || n == -2)
	{
	  fprintf (stderr, "Case conversion produced an invalid character!");
	  abort ();
	}
    }
}

/* Initialize a "struct line" buffer.  Copy multibyte state from `state'
   if not null.  */
static void line_init (struct line *, struct line *, size_t initial_size);
static void
line_init(buf, state, initial_size)
  struct line *buf;
  struct line *state;
  size_t initial_size;
{
  buf->text = MALLOC(initial_size, char);
  buf->active = buf->text;
  buf->alloc = initial_size;
  buf->length = 0;
  buf->chomped = true;

  if (state)
    memcpy (&buf->mbstate, &state->mbstate, sizeof (buf->mbstate));
  else
    memset (&buf->mbstate, 0, sizeof (buf->mbstate));
}

/* Reset a "struct line" buffer to length zero.  Copy multibyte state from
   `state' if not null.  */
static void line_reset (struct line *, struct line *);
static void
line_reset(buf, state)
  struct line *buf, *state;
{
  if (buf->alloc == 0)
    line_init(buf, state, INITIAL_BUFFER_SIZE);
  else
    {
      buf->length = 0;
      if (state)
        memcpy (&buf->mbstate, &state->mbstate, sizeof (buf->mbstate));
      else
        memset (&buf->mbstate, 0, sizeof (buf->mbstate));
    }
}

/* Copy the contents of the line `from' into the line `to'.
   This destroys the old contents of `to'.
   Copy the multibyte state if `state' is true. */
static void line_copy (struct line *from, struct line *to, int state);
static void
line_copy(from, to, state)
  struct line *from;
  struct line *to;
  int state;
{
  /* Remove the inactive portion in the destination buffer. */
  to->alloc += to->active - to->text;

  if (to->alloc < from->length)
    {
      to->alloc *= 2;
      if (to->alloc < from->length)
	to->alloc = from->length;
      if (to->alloc < INITIAL_BUFFER_SIZE)
	to->alloc = INITIAL_BUFFER_SIZE;
      /* Use free()+MALLOC() instead of REALLOC() to
	 avoid unnecessary copying of old text. */
      free(to->text);
      to->text = MALLOC(to->alloc, char);
    }

  to->active = to->text;
  to->length = from->length;
  to->chomped = from->chomped;
  memcpy(to->active, from->active, from->length);

  if (state)
    memcpy(&to->mbstate, &from->mbstate, sizeof (from->mbstate));
}

/* Append the contents of the line `from' to the line `to'.
   Copy the multibyte state if `state' is true. */
static void line_append (struct line *from, struct line *to, int state);
static void
line_append(from, to, state)
  struct line *from;
  struct line *to;
  int state;
{
  str_append(to, &buffer_delimiter, 1);
  str_append(to, from->active, from->length);
  to->chomped = from->chomped;

  if (state)
    memcpy (&to->mbstate, &from->mbstate, sizeof (from->mbstate));
}

/* Exchange two "struct line" buffers.
   Copy the multibyte state if `state' is true. */
static void line_exchange (struct line *a, struct line *b, int state);
static void
line_exchange(a, b, state)
  struct line *a;
  struct line *b;
  int state;
{
  struct line t;

  if (state)
    {
      memcpy(&t,  a, sizeof (struct line));
      memcpy( a,  b, sizeof (struct line));
      memcpy( b, &t, sizeof (struct line));
    }
  else
    {
      memcpy(&t,  a, SIZEOF_LINE);
      memcpy( a,  b, SIZEOF_LINE);
      memcpy( b, &t, SIZEOF_LINE);
    }
}


/* dummy function to simplify read_pattern_space() */
static bool read_always_fail (struct input *);
static bool
read_always_fail(input)
  struct input *input UNUSED;
{
  return false;
}

static bool read_file_line (struct input *);
static bool
read_file_line(input)
  struct input *input;
{
  static char *b;
  static size_t blen;

  long result = ck_getdelim (&b, &blen, buffer_delimiter, input->fp);
  if (result <= 0)
    return false;

  /* Remove the trailing new-line that is left by getline. */
  if (b[result - 1] == buffer_delimiter)
    --result;
  else
    line.chomped = false;

  str_append(&line, b, result);
  return true;
}


static inline void output_missing_newline (struct output *);
static inline void
output_missing_newline(outf)
  struct output *outf;
{
  if (outf->missing_newline)
    {
      ck_fwrite(&buffer_delimiter, 1, 1, outf->fp);
      outf->missing_newline = false;
    }
}

static inline void flush_output (FILE *);
static inline void
flush_output(fp)
  FILE *fp;
{
  if (fp != stdout || unbuffered)
    ck_fflush(fp);
}

static void output_line (const char *, size_t, int, struct output *);
static void
output_line(text, length, nl, outf)
  const char *text;
  size_t length;
  int nl;
  struct output *outf;
{
  if (!text)
    return;

  output_missing_newline(outf);
  if (length)
    ck_fwrite(text, 1, length, outf->fp);
  if (nl)
    ck_fwrite(&buffer_delimiter, 1, 1, outf->fp);
  else
    outf->missing_newline = true;

  flush_output(outf->fp);
}

static struct append_queue *next_append_slot (void);
static struct append_queue *
next_append_slot()
{
  struct append_queue *n = MALLOC(1, struct append_queue);

  n->fname = NULL;
  n->text = NULL;
  n->textlen = 0;
  n->next = NULL;
  n->free = false;

  if (append_tail)
      append_tail->next = n;
  else
      append_head = n;
  return append_tail = n;
}

static void release_append_queue (void);
static void
release_append_queue()
{
  struct append_queue *p, *q;

  for (p=append_head; p; p=q)
    {
      if (p->free)
        free(p->text);

      q = p->next;
      free(p);
    }
  append_head = append_tail = NULL;
}

static void dump_append_queue (void);
static void
dump_append_queue()
{
  struct append_queue *p;

  output_missing_newline(&output_file);
  for (p=append_head; p; p=p->next)
    {
      if (p->text)
        ck_fwrite(p->text, 1, p->textlen, output_file.fp);

      if (p->fname)
	{
	  char buf[FREAD_BUFFER_SIZE];
	  size_t cnt;
	  FILE *fp;

	  /* "If _fname_ does not exist or cannot be read, it shall
	     be treated as if it were an empty file, causing no error
	     condition."  IEEE Std 1003.2-1992
	     So, don't fail. */
	  fp = ck_fopen(p->fname, read_mode, false);
	  if (fp)
	    {
	      while ((cnt = ck_fread(buf, 1, sizeof buf, fp)) > 0)
		ck_fwrite(buf, 1, cnt, output_file.fp);
	      ck_fclose(fp);
	    }
	}
    }

  flush_output(output_file.fp);
  release_append_queue();
}


/* Compute the name of the backup file for in-place editing */
static char *get_backup_file_name (const char *);
static char *
get_backup_file_name(name)
  const char *name;
{
  char *old_asterisk, *asterisk, *backup, *p;
  int name_length = strlen(name), backup_length = strlen(in_place_extension);

  /* Compute the length of the backup file */
  for (asterisk = in_place_extension - 1, old_asterisk = asterisk + 1;
       (asterisk = strchr(old_asterisk, '*'));
       old_asterisk = asterisk + 1)
    backup_length += name_length - 1;

  p = backup = xmalloc(backup_length + 1);

  /* Each iteration gobbles up to an asterisk */
  for (asterisk = in_place_extension - 1, old_asterisk = asterisk + 1;
       (asterisk = strchr(old_asterisk, '*'));
       old_asterisk = asterisk + 1)
    {
      memcpy (p, old_asterisk, asterisk - old_asterisk);
      p += asterisk - old_asterisk;
      strcpy (p, name);
      p += name_length;
    }

  /* Tack on what's after the last asterisk */
  strcpy (p, old_asterisk);
  return backup;
}

/* Initialize a struct input for the named file. */
static void open_next_file (const char *name, struct input *);
static void
open_next_file(name, input)
  const char *name;
  struct input *input;
{
  buffer.length = 0;

  if (name[0] == '-' && name[1] == '\0' && !in_place_extension)
    {
      clearerr(stdin);	/* clear any stale EOF indication */
#if defined(WIN32) || defined(_WIN32) || defined(__CYGWIN__) || defined(MSDOS) || defined(__EMX__)
      input->fp = ck_fdopen (fileno (stdin), "stdin", read_mode, false);
#else
      input->fp = stdin;
#endif
    }
  else if ( ! (input->fp = ck_fopen(name, read_mode, false)) )
    {
      const char *ptr = strerror(errno);
      fprintf(stderr, _("%s: can't read %s: %s\n"), myname, name, ptr);
      input->read_fn = read_always_fail; /* a redundancy */
      ++input->bad_count;
      return;
    }

  input->read_fn = read_file_line;

  if (follow_symlinks)
    input->in_file_name = follow_symlink (name);
  else
    input->in_file_name = name;

  if (in_place_extension)
    {
      int input_fd;
      char *tmpdir, *p;
      security_context_t old_fscreatecon;
      int reset_fscreatecon = 0;
      memset (&old_fscreatecon, 0, sizeof (old_fscreatecon));

      /* get the base name */
      tmpdir = ck_strdup(input->in_file_name);
      if ((p = strrchr(tmpdir, '/')))
	*p = 0;
      else
	strcpy(tmpdir, ".");

      if (isatty (fileno (input->fp)))
        panic(_("couldn't edit %s: is a terminal"), input->in_file_name);

      input_fd = fileno (input->fp);
      fstat (input_fd, &input->st);
      if (!S_ISREG (input->st.st_mode))
        panic(_("couldn't edit %s: not a regular file"), input->in_file_name);

      if (is_selinux_enabled () > 0)
	{
          security_context_t con;
	  if (getfilecon (input->in_file_name, &con) != -1)
	    {
	      /* Save and restore the old context for the sake of w and W
		 commands.  */
	      reset_fscreatecon = getfscreatecon (&old_fscreatecon) >= 0;
	      if (setfscreatecon (con) < 0)
		fprintf (stderr, _("%s: warning: failed to set default file creation context to %s: %s"),
			 myname, con, strerror (errno));
	      freecon (con);
	    }
	  else
	    {
	      if (errno != ENOSYS)
		fprintf (stderr, _("%s: warning: failed to get security context of %s: %s"),
			 myname, input->in_file_name, strerror (errno));
	    }
	}

      output_file.fp = ck_mkstemp (&input->out_file_name, tmpdir, "sed",
				   write_mode);
      output_file.missing_newline = false;
      free (tmpdir);

      if (reset_fscreatecon)
	{
	  setfscreatecon (old_fscreatecon);
	  freecon (old_fscreatecon);
	}

      if (!output_file.fp)
        panic(_("couldn't open temporary file %s: %s"), input->out_file_name, strerror(errno));
    }
  else
    {
      if (input->fp && unbuffered)
        setvbuf (input->fp, NULL, _IONBF, 0);
      output_file.fp = stdout;
    }
}


/* Clean up an input stream that we are done with. */
static void closedown (struct input *);
static void
closedown(input)
  struct input *input;
{
  input->read_fn = read_always_fail;
  if (!input->fp)
    return;

  if (in_place_extension && output_file.fp != NULL)
    {
      const char *target_name;
      int input_fd, output_fd;

      target_name = input->in_file_name;
      input_fd = fileno (input->fp);
      output_fd = fileno (output_file.fp);
#ifdef HAVE_FCHOWN
      if (fchown (output_fd, input->st.st_uid, input->st.st_gid) == -1)
        fchown (output_fd, -1, input->st.st_gid);
#endif
      copy_acl (input->in_file_name, input_fd,
		input->out_file_name, output_fd,
		input->st.st_mode);

      ck_fclose (input->fp);
      ck_fclose (output_file.fp);
      if (strcmp(in_place_extension, "*") != 0)
        {
          char *backup_file_name = get_backup_file_name(target_name);
	  ck_rename (target_name, backup_file_name, input->out_file_name);
          free (backup_file_name);
	}

      ck_rename (input->out_file_name, target_name, input->out_file_name);
      free (input->out_file_name);
    }
  else
    ck_fclose (input->fp);

  input->fp = NULL;
}

/* Reset range commands so that they are marked as non-matching */
static void reset_addresses (struct vector *);
static void
reset_addresses(vec)
     struct vector *vec;
{
  struct sed_cmd *cur_cmd;
  int n;

  for (cur_cmd = vec->v, n = vec->v_length; n--; cur_cmd++)
    if (cur_cmd->a1
	&& cur_cmd->a1->addr_type == ADDR_IS_NUM
	&& cur_cmd->a1->addr_number == 0)
      cur_cmd->range_state = RANGE_ACTIVE;
    else
      cur_cmd->range_state = RANGE_INACTIVE;
}

/* Read in the next line of input, and store it in the pattern space.
   Return zero if there is nothing left to input. */
static bool read_pattern_space (struct input *, struct vector *, int);
static bool
read_pattern_space(input, the_program, append)
  struct input *input;
  struct vector *the_program;
  int append;
{
  if (append_head) /* redundant test to optimize for common case */
    dump_append_queue();
  replaced = false;
  if (!append)
    line.length = 0;
  line.chomped = true;  /* default, until proved otherwise */

  while ( ! (*input->read_fn)(input) )
    {
      closedown(input);

      if (!*input->file_list)
	return false;

      if (input->reset_at_next_file)
	{
	  input->line_number = 0;
	  hold.length = 0;
	  reset_addresses (the_program);
	  rewind_read_files ();

	  /* If doing in-place editing, we will never append the
	     new-line to this file; but if the output goes to stdout,
	     we might still have to output the missing new-line.  */
	  if (in_place_extension)
	    output_file.missing_newline = false;

	  input->reset_at_next_file = separate_files;
	}

      open_next_file (*input->file_list++, input);
    }

  ++input->line_number;
  return true;
}


static bool last_file_with_data_p (struct input *);
static bool
last_file_with_data_p(input)
  struct input *input;
{
  for (;;)
    {
      int ch;

      closedown(input);
      if (!*input->file_list)
	return true;
      open_next_file(*input->file_list++, input);
      if (input->fp)
	{
	  if ((ch = getc(input->fp)) != EOF)
	    {
	      ungetc(ch, input->fp);
	      return false;
	    }
	}
    }
}

/* Determine if we match the `$' address. */
static bool test_eof (struct input *);
static bool
test_eof(input)
  struct input *input;
{
  int ch;

  if (buffer.length)
    return false;
  if (!input->fp)
    return separate_files || last_file_with_data_p(input);
  if (feof(input->fp))
    return separate_files || last_file_with_data_p(input);
  if ((ch = getc(input->fp)) == EOF)
    return separate_files || last_file_with_data_p(input);
  ungetc(ch, input->fp);
  return false;
}

/* Return non-zero if the current line matches the address
   pointed to by `addr'. */
static bool match_an_address_p (struct addr *, struct input *);
static bool
match_an_address_p(addr, input)
  struct addr *addr;
  struct input *input;
{
  switch (addr->addr_type)
    {
    case ADDR_IS_NULL:
      return true;

    case ADDR_IS_REGEX:
      return match_regex(addr->addr_regex, line.active, line.length, 0, NULL, 0);

    case ADDR_IS_NUM_MOD:
      return (input->line_number >= addr->addr_number
	      && ((input->line_number - addr->addr_number) % addr->addr_step) == 0);

    case ADDR_IS_STEP:
    case ADDR_IS_STEP_MOD:
      /* reminder: these are only meaningful for a2 addresses */
      /* a2->addr_number needs to be recomputed each time a1 address
         matches for the step and step_mod types */
      return (addr->addr_number <= input->line_number);

    case ADDR_IS_LAST:
      return test_eof(input);

      /* ADDR_IS_NUM is handled in match_address_p.  */
    case ADDR_IS_NUM:
    default:
      panic("INTERNAL ERROR: bad address type");
    }
  /*NOTREACHED*/
  return false;
}

/* return non-zero if current address is valid for cmd */
static bool match_address_p (struct sed_cmd *, struct input *);
static bool
match_address_p(cmd, input)
  struct sed_cmd *cmd;
  struct input *input;
{
  if (!cmd->a1)
    return true;

  if (cmd->range_state != RANGE_ACTIVE)
    {
      /* Find if we are going to activate a range.  Handle ADDR_IS_NUM
	 specially: it represent an "absolute" state, it should not
	 be computed like regexes.  */
      if (cmd->a1->addr_type == ADDR_IS_NUM)
	{
	  if (!cmd->a2)
	    return (input->line_number == cmd->a1->addr_number);

	  if (cmd->range_state == RANGE_CLOSED
	      || input->line_number < cmd->a1->addr_number)
	    return false;
	}
      else
	{
          if (!cmd->a2)
	    return match_an_address_p(cmd->a1, input);

	  if (!match_an_address_p(cmd->a1, input))
            return false;
	}

      /* Ok, start a new range.  */
      cmd->range_state = RANGE_ACTIVE;
      switch (cmd->a2->addr_type)
	{
	case ADDR_IS_REGEX:
	  /* Always include at least two lines.  */
	  return true;
	case ADDR_IS_NUM:
	  /* Same handling as below, but always include at least one line.  */
          if (input->line_number >= cmd->a2->addr_number)
	    cmd->range_state = RANGE_CLOSED;
          return true;
	case ADDR_IS_STEP:
	  cmd->a2->addr_number = input->line_number + cmd->a2->addr_step;
	  return true;
	case ADDR_IS_STEP_MOD:
	  cmd->a2->addr_number = input->line_number + cmd->a2->addr_step
				 - (input->line_number%cmd->a2->addr_step);
	  return true;
	default:
	  break;
        }
    }

  /* cmd->range_state == RANGE_ACTIVE.  Check if the range is
     ending; also handle ADDR_IS_NUM specially in this case.  */

  if (cmd->a2->addr_type == ADDR_IS_NUM)
    {
      /* If the second address is a line number, and if we got past
         that line, fail to match (it can happen when you jump
	 over such addresses with `b' and `t'.  Use RANGE_CLOSED
         so that the range is not re-enabled anymore.  */
      if (input->line_number >= cmd->a2->addr_number)
	cmd->range_state = RANGE_CLOSED;

      return (input->line_number <= cmd->a2->addr_number);
   }

  /* Other addresses are treated as usual.  */
  if (match_an_address_p(cmd->a2, input))
    cmd->range_state = RANGE_CLOSED;

  return true;
}


static void do_list (int line_len);
static void
do_list(line_len)
     int line_len;
{
  unsigned char *p = (unsigned char *)line.active;
  countT len = line.length;
  countT width = 0;
  char obuf[180];	/* just in case we encounter a 512-bit char (;-) */
  char *o;
  size_t olen;
  FILE *fp = output_file.fp;

  output_missing_newline(&output_file);
  for (; len--; ++p) {
      o = obuf;
      
      /* Some locales define 8-bit characters as printable.  This makes the
	 testsuite fail at 8to7.sed because the `l' command in fact will not
	 convert the 8-bit characters. */
#if defined isascii || defined HAVE_ISASCII
      if (isascii(*p) && ISPRINT(*p)) {
#else
      if (ISPRINT(*p)) {
#endif
	  *o++ = *p;
	  if (*p == '\\')
	    *o++ = '\\';
      } else {
	  *o++ = '\\';
	  switch (*p) {
#if defined __STDC__ && __STDC__-0
	    case '\a': *o++ = 'a'; break;
#else /* Not STDC; we'll just assume ASCII */
	    case 007:  *o++ = 'a'; break;
#endif
	    case '\b': *o++ = 'b'; break;
	    case '\f': *o++ = 'f'; break;
	    case '\n': *o++ = 'n'; break;
	    case '\r': *o++ = 'r'; break;
	    case '\t': *o++ = 't'; break;
	    case '\v': *o++ = 'v'; break;
	    default:
	      sprintf(o, "%03o", *p);
	      o += strlen(o);
	      break;
	    }
      }
      olen = o - obuf;
      if (width+olen >= line_len && line_len > 0) {
	  ck_fwrite("\\\n", 1, 2, fp);
	  width = 0;
      }
      ck_fwrite(obuf, 1, olen, fp);
      width += olen;
  }
  ck_fwrite("$\n", 1, 2, fp);
  flush_output (fp);
}


static void append_replacement (struct line *buf, struct replacement *p,
				struct re_registers *regs)
{
  enum replacement_types repl_mod = 0;

  for (; p; p=p->next)
    {
      int i = p->subst_id;
      enum replacement_types curr_type;

      /* Apply a \[lu] modifier that was given earlier, but which we
         have not had yet the occasion to apply.  But don't do it
         if this replacement has a modifier of its own. */
      curr_type = (p->repl_type & REPL_MODIFIERS)
        ? p->repl_type
        : p->repl_type | repl_mod;

      repl_mod = 0;
      if (p->prefix_length)
        {
          str_append_modified(buf, p->prefix, p->prefix_length,
    			      curr_type);
          curr_type &= ~REPL_MODIFIERS;
        }

      if (0 <= i)
	{
          if (regs->end[i] == regs->start[i] && p->repl_type & REPL_MODIFIERS)
            /* Save this modifier, we shall apply it later.
	       e.g. in s/()([a-z])/\u\1\2/
	       the \u modifier is applied to \2, not \1 */
	    repl_mod = curr_type & REPL_MODIFIERS;

	  else if (regs->end[i] != regs->start[i])
	    str_append_modified(buf, line.active + regs->start[i],
			        (size_t)(regs->end[i] - regs->start[i]),
			        curr_type);
	}
    }
}

static void do_subst (struct subst *);
static void
do_subst(sub)
  struct subst *sub;
{
  size_t start = 0;	/* where to start scan for (next) match in LINE */
  size_t last_end = 0;  /* where did the last successful match end in LINE */
  countT count = 0;	/* number of matches found */
  bool again = true;

  static struct re_registers regs;

  line_reset(&s_accum, &line);

  /* The first part of the loop optimizes s/xxx// when xxx is at the
     start, and s/xxx$// */
  if (!match_regex(sub->regx, line.active, line.length, start,
		   &regs, sub->max_id + 1))
    return;
  
  if (!sub->replacement && sub->numb <= 1)
    {
      if (regs.start[0] == 0 && !sub->global)
        {
	  /* We found a match, set the `replaced' flag. */
	  replaced = true;

	  line.active += regs.end[0];
	  line.length -= regs.end[0];
	  line.alloc -= regs.end[0];
	  goto post_subst;
        }
      else if (regs.end[0] == line.length)
        {
	  /* We found a match, set the `replaced' flag. */
	  replaced = true;

	  line.length = regs.start[0];
	  goto post_subst;
        }
    }

  do
    {
      size_t offset = regs.start[0];
      size_t matched = regs.end[0] - regs.start[0];

      /* Copy stuff to the left of this match into the output string. */
      if (start < offset)
	str_append(&s_accum, line.active + start, offset - start);

      /* If we're counting up to the Nth match, are we there yet?
         And even if we are there, there is another case we have to
	 skip: are we matching an empty string immediately following
         another match?

         This latter case avoids that baaaac, when passed through
         s,a*,x,g, gives `xbxxcx' instead of xbxcx.  This behavior is
         unacceptable because it is not consistently applied (for
         example, `baaaa' gives `xbx', not `xbxx'). */
      if ((matched > 0 || count == 0 || offset > last_end)
	  && ++count >= sub->numb)
        {
          /* We found a match, set the `replaced' flag. */
          replaced = true;

          /* Now expand the replacement string into the output string. */
          append_replacement (&s_accum, sub->replacement, &regs);
	  again = sub->global;
        }
      else
	{
          /* The match was not replaced.  Copy the text until its
             end; if it was vacuous, skip over one character and
	     add that character to the output.  */
	  if (matched == 0)
	    {
	      if (start < line.length)
	        matched = 1;
	      else
	        break;
	    }

	  str_append(&s_accum, line.active + offset, matched);
        }

      /* Start after the match.  last_end is the real end of the matched
	 substring, excluding characters that were skipped in case the RE
	 matched the empty string.  */
      start = offset + matched;
      last_end = regs.end[0];
    }
  while (again
	 && start <= line.length
	 && match_regex(sub->regx, line.active, line.length, start,
			&regs, sub->max_id + 1));

  /* Copy stuff to the right of the last match into the output string. */
  if (start < line.length)
    str_append(&s_accum, line.active + start, line.length-start);
  s_accum.chomped = line.chomped;

  /* Exchange line and s_accum.  This can be much cheaper
     than copying s_accum.active into line.text (for huge lines). */
  line_exchange(&line, &s_accum, false);

  /* Finish up. */
  if (count < sub->numb)
    return;

 post_subst:
  if (sub->print & 1)
    output_line(line.active, line.length, line.chomped, &output_file);
  
  if (sub->eval) 
    {
#ifdef HAVE_POPEN
      FILE *pipe_fp;
      line_reset(&s_accum, NULL);
      
      str_append (&line, "", 1);
      pipe_fp = popen(line.active, "r");
      
      if (pipe_fp != NULL) 
	{
	  while (!feof (pipe_fp)) 
	    {
	      char buf[4096];
	      int n = fread (buf, sizeof(char), 4096, pipe_fp);
	      if (n > 0)
		str_append(&s_accum, buf, n);
	    }
	  
	  pclose (pipe_fp);

	  /* Exchange line and s_accum.  This can be much cheaper than copying
	     s_accum.active into line.text (for huge lines).  See comment above
	     for 'g' as to while the third argument is incorrect anyway.  */
	  line_exchange(&line, &s_accum, true);
	  if (line.length &&
	      line.active[line.length - 1] == buffer_delimiter)
	    line.length--;
	}
      else
	panic(_("error in subprocess"));
#else
      panic(_("option `e' not supported"));
#endif
    } 
  
  if (sub->print & 2)
    output_line(line.active, line.length, line.chomped, &output_file);
  if (sub->outf)
    output_line(line.active, line.length, line.chomped, sub->outf);
}

#ifdef EXPERIMENTAL_DASH_N_OPTIMIZATION
/* Used to attempt a simple-minded optimization. */

static countT branches;

static countT count_branches (struct vector *);
static countT
count_branches(program)
  struct vector *program;
{
  struct sed_cmd *cur_cmd = program->v;
  countT isn_cnt = program->v_length;
  countT cnt = 0;

  while (isn_cnt-- > 0)
    {
      switch (cur_cmd->cmd)
	{
	case 'b':
	case 't':
	case 'T':
	case '{':
	  ++cnt;
	}
    }
  return cnt;
}

static struct sed_cmd *shrink_program (struct vector *, struct sed_cmd *);
static struct sed_cmd *
shrink_program(vec, cur_cmd)
  struct vector *vec;
  struct sed_cmd *cur_cmd;
{
  struct sed_cmd *v = vec->v;
  struct sed_cmd *last_cmd = v + vec->v_length;
  struct sed_cmd *p;
  countT cmd_cnt;

  for (p=v; p < cur_cmd; ++p)
    if (p->cmd != '#')
      memcpy(v++, p, sizeof *v);
  cmd_cnt = v - vec->v;

  for (; p < last_cmd; ++p)
    if (p->cmd != '#')
      memcpy(v++, p, sizeof *v);
  vec->v_length = v - vec->v;

  return (0 < vec->v_length) ? (vec->v + cmd_cnt) : (struct sed_cmd *)0;
}
#endif /*EXPERIMENTAL_DASH_N_OPTIMIZATION*/

/* Execute the program `vec' on the current input line.
   Return exit status if caller should quit, -1 otherwise. */
static int execute_program (struct vector *, struct input *);
static int
execute_program(vec, input)
  struct vector *vec;
  struct input *input;
{
  struct sed_cmd *cur_cmd;
  struct sed_cmd *end_cmd;

  cur_cmd = vec->v;
  end_cmd = vec->v + vec->v_length;
  while (cur_cmd < end_cmd)
    {
      if (match_address_p(cur_cmd, input) != cur_cmd->addr_bang)
	{
	  switch (cur_cmd->cmd)
	    {
	    case 'a':
	      {
		struct append_queue *aq = next_append_slot();
		aq->text = cur_cmd->x.cmd_txt.text;
		aq->textlen = cur_cmd->x.cmd_txt.text_length;
	      }
	      break;

	    case '{':
	    case 'b':
	      cur_cmd = vec->v + cur_cmd->x.jump_index;
	      continue;

	    case '}':
	    case '#':
	    case ':':
	      /* Executing labels and block-ends are easy. */
	      break;

	    case 'c':
	      if (cur_cmd->range_state != RANGE_ACTIVE)
		output_line(cur_cmd->x.cmd_txt.text,
			    cur_cmd->x.cmd_txt.text_length - 1, true,
			    &output_file);
	      /* POSIX.2 is silent about c starting a new cycle,
		 but it seems to be expected (and make sense). */
	      /* Fall Through */
	    case 'd':
	      return -1;

	    case 'D':
	      {
		char *p = memchr(line.active, buffer_delimiter, line.length);
		if (!p)
		  return -1;

		++p;
		line.alloc -= p - line.active;
		line.length -= p - line.active;
		line.active += p - line.active;

		/* reset to start next cycle without reading a new line: */
		cur_cmd = vec->v;
		continue;
	      }

	    case 'e': {
#ifdef HAVE_POPEN
	      FILE *pipe_fp;
	      int cmd_length = cur_cmd->x.cmd_txt.text_length;
	      line_reset(&s_accum, NULL);

	      if (!cmd_length)
		{
		  str_append (&line, "", 1);
		  pipe_fp = popen(line.active, "r");
		} 
	      else
		{
		  cur_cmd->x.cmd_txt.text[cmd_length - 1] = 0;
		  pipe_fp = popen(cur_cmd->x.cmd_txt.text, "r");
                  output_missing_newline(&output_file);
		}

	      if (pipe_fp != NULL) 
		{
		  char buf[4096];
		  int n;
		  while (!feof (pipe_fp)) 
		    if ((n = fread (buf, sizeof(char), 4096, pipe_fp)) > 0)
		      {
			if (!cmd_length)
			  str_append(&s_accum, buf, n);
			else
			  ck_fwrite(buf, 1, n, output_file.fp);
		      }
		  
		  pclose (pipe_fp);
		  if (!cmd_length)
		    {
		      /* Store into pattern space for plain `e' commands */
		      if (s_accum.length &&
			  s_accum.active[s_accum.length - 1] == buffer_delimiter)
			s_accum.length--;

		      /* Exchange line and s_accum.  This can be much
			 cheaper than copying s_accum.active into line.text
			 (for huge lines).  See comment above for 'g' as
			 to while the third argument is incorrect anyway.  */
		      line_exchange(&line, &s_accum, true);
		    }
                  else
                    flush_output(output_file.fp);

		}
	      else
		panic(_("error in subprocess"));
#else
	      panic(_("`e' command not supported"));
#endif
	      break;
	    }

	    case 'g':
	      /* We do not have a really good choice for the third parameter.
		 The problem is that hold space and the input file might as
		 well have different states; copying it from hold space means
		 that subsequent input might be read incorrectly, while
		 keeping it as in pattern space means that commands operating
		 on the moved buffer might consider a wrong character set.
		 We keep it true because it's what sed <= 4.1.5 did.  */
	      line_copy(&hold, &line, true);
	      break;

	    case 'G':
	      /* We do not have a really good choice for the third parameter.
		 The problem is that hold space and pattern space might as
		 well have different states.  So, true is as wrong as false.
		 We keep it true because it's what sed <= 4.1.5 did, but
		 we could consider having line_ap.  */
	      line_append(&hold, &line, true);
	      break;

	    case 'h':
	      /* Here, it is ok to have true.  */
	      line_copy(&line, &hold, true);
	      break;

	    case 'H':
	      /* See comment above for 'G' regarding the third parameter.  */
	      line_append(&line, &hold, true);
	      break;

	    case 'i':
	      output_line(cur_cmd->x.cmd_txt.text,
			  cur_cmd->x.cmd_txt.text_length - 1,
			  true, &output_file);
	      break;

	    case 'l':
	      do_list(cur_cmd->x.int_arg == -1
		      ? lcmd_out_line_len
		      : cur_cmd->x.int_arg);
	      break;

	    case 'L':
              output_missing_newline(&output_file);
	      fmt(line.active, line.active + line.length,
		  cur_cmd->x.int_arg == -1
		  ? lcmd_out_line_len
		  : cur_cmd->x.int_arg,
		  output_file.fp);
              flush_output(output_file.fp);
	      break;

	    case 'n':
	      if (!no_default_output)
		output_line(line.active, line.length, line.chomped, &output_file);
	      if (test_eof(input) || !read_pattern_space(input, vec, false))
		return -1;
	      break;

	    case 'N':
	      str_append(&line, &buffer_delimiter, 1);
 
              if (test_eof(input) || !read_pattern_space(input, vec, true))
                {
                  line.length--;
                  if (posixicity == POSIXLY_EXTENDED && !no_default_output)
                     output_line(line.active, line.length, line.chomped,
                                 &output_file);
                  return -1;
                }
	      break;

	    case 'p':
	      output_line(line.active, line.length, line.chomped, &output_file);
	      break;

	    case 'P':
	      {
		char *p = memchr(line.active, buffer_delimiter, line.length);
		output_line(line.active, p ? p - line.active : line.length,
			    p ? true : line.chomped, &output_file);
	      }
	      break;

            case 'q':
              if (!no_default_output)
                output_line(line.active, line.length, line.chomped, &output_file);
	      dump_append_queue();

	    case 'Q':
	      return cur_cmd->x.int_arg == -1 ? 0 : cur_cmd->x.int_arg;

	    case 'r':
	      if (cur_cmd->x.fname)
		{
		  struct append_queue *aq = next_append_slot();
		  aq->fname = cur_cmd->x.fname;
		}
	      break;

	    case 'R':
	      if (cur_cmd->x.fp && !feof (cur_cmd->x.fp))
		{
		  struct append_queue *aq;
		  size_t buflen;
		  char *text = NULL;
		  int result;

		  result = ck_getdelim (&text, &buflen, buffer_delimiter,
				        cur_cmd->x.fp);
		  if (result != EOF)
		    {
		      aq = next_append_slot();
		      aq->free = true;
		      aq->text = text;
		      aq->textlen = result;
		    }
		}
	      break;

	    case 's':
	      do_subst(cur_cmd->x.cmd_subst);
	      break;

	    case 't':
	      if (replaced)
		{
		  replaced = false;
		  cur_cmd = vec->v + cur_cmd->x.jump_index;
		  continue;
		}
	      break;

	    case 'T':
	      if (!replaced)
		{
		  cur_cmd = vec->v + cur_cmd->x.jump_index;
		  continue;
		}
	      else
		replaced = false;
	      break;

	    case 'w':
	      if (cur_cmd->x.fp)
		output_line(line.active, line.length,
			    line.chomped, cur_cmd->x.outf);
	      break;

	    case 'W':
	      if (cur_cmd->x.fp)
	        {
		  char *p = memchr(line.active, buffer_delimiter, line.length);
		  output_line(line.active, p ? p - line.active : line.length,
			      p ? true : line.chomped, cur_cmd->x.outf);
	        }
	      break;

	    case 'x':
	      /* See comment above for 'g' regarding the third parameter.  */
	      line_exchange(&line, &hold, false);
	      break;

	    case 'y':
	      {
               if (mb_cur_max > 1)
                 {
                   int idx, prev_idx; /* index in the input line.  */
                   char **trans;
                   mbstate_t mbstate;
                   memset(&mbstate, 0, sizeof(mbstate_t));
                   for (idx = 0; idx < line.length;)
                     {
                       int mbclen, i;
                       mbclen = MBRLEN (line.active + idx, line.length - idx,
                                          &mbstate);
                       /* An invalid sequence, or a truncated multibyte
                          character.  We treat it as a singlebyte character.
                       */
                       if (mbclen == (size_t) -1 || mbclen == (size_t) -2
                           || mbclen == 0)
                         mbclen = 1;

                       trans = cur_cmd->x.translatemb;
                       /* `i' indicate i-th translate pair.  */
                       for (i = 0; trans[2*i] != NULL; i++)
                         {
                           if (strncmp(line.active + idx, trans[2*i], mbclen) == 0)
                             {
                               bool move_remain_buffer = false;
                               int trans_len = strlen(trans[2*i+1]);

                               if (mbclen < trans_len)
                                 {
                                   int new_len;
                                   new_len = line.length + 1 + trans_len - mbclen;
                                   /* We must extend the line buffer.  */
                                   if (line.alloc < new_len)
                                     {
                                       /* And we must resize the buffer.  */
                                       resize_line(&line, new_len);
                                     }
                                   move_remain_buffer = true;
                                 }
                               else if (mbclen > trans_len)
                                 {
                                   /* We must truncate the line buffer.  */
                                   move_remain_buffer = true;
                                 }
                               prev_idx = idx;
                               if (move_remain_buffer)
                                 {
                                   int move_len, move_offset;
                                   char *move_from, *move_to;
                                   /* Move the remaining with \0.  */
                                   move_from = line.active + idx + mbclen;
                                   move_to = line.active + idx + trans_len;
                                   move_len = line.length + 1 - idx - mbclen;
                                   move_offset = trans_len - mbclen;
                                   memmove(move_to, move_from, move_len);
                                   line.length += move_offset;
                                   idx += move_offset;
                                 }
                               strncpy(line.active + prev_idx, trans[2*i+1],
                                       trans_len);
                               break;
                             }
                         }
                       idx += mbclen;
                     }
                 }
               else
                 {
                   unsigned char *p, *e;
                   p = (unsigned char *)line.active;
                   for (e=p+line.length; p<e; ++p)
                     *p = cur_cmd->x.translate[*p];
                 }
	      }
	      break;

	    case 'z':
	      line.length = 0;
	      break;

	    case '=':
              output_missing_newline(&output_file);
              fprintf(output_file.fp, "%lu\n",
                      (unsigned long)input->line_number);
              flush_output(output_file.fp);
             break;

           case 'F':
              output_missing_newline(&output_file);
              fprintf(output_file.fp, "%s\n",
                      input->in_file_name);
              flush_output(output_file.fp);
             break;

           default:
             panic("INTERNAL ERROR: Bad cmd %c", cur_cmd->cmd);
           }
	}

#ifdef EXPERIMENTAL_DASH_N_OPTIMIZATION
      /* If our top-level program consists solely of commands with
         ADDR_IS_NUM addresses then once we past the last mentioned
         line we should be able to quit if no_default_output is true,
         or otherwise quickly copy input to output.  Now whether this
         optimization is a win or not depends on how cheaply we can
         implement this for the cases where it doesn't help, as
         compared against how much time is saved.  One semantic
         difference (which I think is an improvement) is that *this*
         version will terminate after printing line two in the script
         "yes | sed -n 2p". 
        
         Don't use this when in-place editing is active, because line
         numbers restart each time then. */
      else if (!separate_files)
	{
	  if (cur_cmd->a1->addr_type == ADDR_IS_NUM
	      && (cur_cmd->a2
		  ? cur_cmd->range_state == RANGE_CLOSED
		  : cur_cmd->a1->addr_number < input->line_number))
	    {
	      /* Skip this address next time */
	      cur_cmd->addr_bang = !cur_cmd->addr_bang;
	      cur_cmd->a1->addr_type = ADDR_IS_NULL;
	      if (cur_cmd->a2)
		cur_cmd->a2->addr_type = ADDR_IS_NULL;

	      /* can we make an optimization? */
	      if (cur_cmd->addr_bang)
		{
		  if (cur_cmd->cmd == 'b' || cur_cmd->cmd == 't'
		      || cur_cmd->cmd == 'T' || cur_cmd->cmd == '}')
		    branches--;

		  cur_cmd->cmd = '#';	/* replace with no-op */
	          if (branches == 0)
		    cur_cmd = shrink_program(vec, cur_cmd);
		  if (!cur_cmd && no_default_output)
		    return 0;
		  end_cmd = vec->v + vec->v_length;
		  if (!cur_cmd)
		    cur_cmd = end_cmd;
		  continue;
		}
	    }
	}
#endif /*EXPERIMENTAL_DASH_N_OPTIMIZATION*/

      /* this is buried down here so that a "continue" statement can skip it */
      ++cur_cmd;
    }

    if (!no_default_output)
      output_line(line.active, line.length, line.chomped, &output_file);
    return -1;
}



/* Apply the compiled script to all the named files. */
int
process_files(the_program, argv)
  struct vector *the_program;
  char **argv;
{
  static char dash[] = "-";
  static char *stdin_argv[2] = { dash, NULL };
  struct input input;
  int status;

  line_init(&line, NULL, INITIAL_BUFFER_SIZE);
  line_init(&hold, NULL, 0);
  line_init(&buffer, NULL, 0);

#ifdef EXPERIMENTAL_DASH_N_OPTIMIZATION
  branches = count_branches(the_program);
#endif /*EXPERIMENTAL_DASH_N_OPTIMIZATION*/
  input.reset_at_next_file = true;
  if (argv && *argv)
    input.file_list = argv;
  else if (in_place_extension)
    panic(_("no input files"));
  else
    input.file_list = stdin_argv;

  input.bad_count = 0;
  input.line_number = 0;
  input.read_fn = read_always_fail;
  input.fp = NULL;

  status = EXIT_SUCCESS;
  while (read_pattern_space(&input, the_program, false))
    {
      status = execute_program(the_program, &input);
      if (status == -1)
	status = EXIT_SUCCESS;
      else
	break;
    }
  closedown(&input);

#ifdef DEBUG_LEAKS
  /* We're about to exit, so these free()s are redundant.
     But if we're running under a memory-leak detecting
     implementation of malloc(), we want to explicitly
     deallocate in order to avoid extraneous noise from
     the allocator. */
  release_append_queue();
  free(buffer.text);
  free(hold.text);
  free(line.text);
  free(s_accum.text);
#endif /*DEBUG_LEAKS*/

  if (input.bad_count)
    status = 2;

  return status;
}
