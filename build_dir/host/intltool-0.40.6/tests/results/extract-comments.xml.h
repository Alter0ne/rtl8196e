/*  This comment is not ignored even if it contains > (greater than) 
sign in it, and the following string shows up as well  */
char *s = N_("Dum-dee-dum");
/*  If comment doesn't end on the previous line, it should be handled
     correctly  */
char *s = N_("Final thing to translate");
/*  This comment is not ignored  */
char *s = N_("First thing to translate");
/*  This is multi line comment for the following string;
     second line of multi-line comment  */
char *s = N_("Something to translate");
char *s = N_("This doesn't have a comment for translators");
/*  Comment for *both* attributes and content  */
char *s = N_("attribute value");
/*  This comment will appear if attributes can be commented about  */
char *s = N_("attribute with comment");
char *s = N_("attribute without comment");
/*  Comment for *both* attributes and content  */
char *s = N_("even more content");
