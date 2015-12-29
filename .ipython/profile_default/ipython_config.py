# Configuration file for ipython.

#------------------------------------------------------------------------------
# Configurable configuration
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# InteractiveShellApp configuration
#------------------------------------------------------------------------------

# A Mixin for applications that start InteractiveShell instances.
#
# Provides configurables for loading extensions and executing files as part of
# configuring a Shell environment.
#
# The following methods should be called by the :meth:`initialize` method of the
# subclass:
#
#   - :meth:`init_path`
#   - :meth:`init_shell` (to be implemented by the subclass)
#   - :meth:`init_gui_pylab`
#   - :meth:`init_extensions`
#   - :meth:`init_code`

# Execute the given command string.
#c.InteractiveShellApp.code_to_run = 'autoreload'

# Reraise exceptions encountered loading IPython extensions?
# c.InteractiveShellApp.reraise_ipython_extension_failures = False

# Run the file referenced by the PYTHONSTARTUP environment variable at IPython
# startup.
# c.InteractiveShellApp.exec_PYTHONSTARTUP = True

# lines of code to run at IPython startup.
# c.InteractiveShellApp.exec_lines = traitlets.Undefined

# Enable GUI event loop integration with any of ('glut', 'gtk', 'gtk3', 'osx',
# 'pyglet', 'qt', 'qt5', 'tk', 'wx').
# c.InteractiveShellApp.gui = None

# Pre-load matplotlib and numpy for interactive use, selecting a particular
# matplotlib backend and loop integration.
# c.InteractiveShellApp.pylab = None

# Configure matplotlib for interactive use with the default matplotlib backend.
# c.InteractiveShellApp.matplotlib = None

# If true, IPython will populate the user namespace with numpy, pylab, etc. and
# an ``import *`` is done from numpy and pylab, when using pylab mode.
#
# When False, pylab mode should not import any names into the user namespace.
# c.InteractiveShellApp.pylab_import_all = True

# A list of dotted module names of IPython extensions to load.
# c.InteractiveShellApp.extensions = traitlets.Undefined

# Run the module as a script.
# c.InteractiveShellApp.module_to_run = ''

# Should variables loaded at startup (by startup files, exec_lines, etc.) be
# hidden from tools like %who?
c.InteractiveShellApp.hide_initial_ns = False

# dotted module name of an IPython extension to load.
c.InteractiveShellApp.extra_extension = 'autoreload'

# List of files to run at IPython startup.
# c.InteractiveShellApp.exec_files = traitlets.Undefined

# A file to be run
# c.InteractiveShellApp.file_to_run = ''

#------------------------------------------------------------------------------
# SingletonConfigurable configuration
#------------------------------------------------------------------------------

# A configurable that only allows one instance.
#
# This class is for classes that should only have one instance of itself or
# *any* subclass. To create and retrieve such a class use the
# :meth:`SingletonConfigurable.instance` method.

#------------------------------------------------------------------------------
# Application configuration
#------------------------------------------------------------------------------

# This is an application.

# The date format used by logging formatters for %(asctime)s
# c.Application.log_datefmt = '%Y-%m-%d %H:%M:%S'

# The Logging format template
# c.Application.log_format = '[%(name)s]%(highlevel)s %(message)s'

# Set the log level by value or name.
# c.Application.log_level = 30

#------------------------------------------------------------------------------
# BaseIPythonApplication configuration
#------------------------------------------------------------------------------

# IPython: an enhanced interactive Python shell.

# The IPython profile to use.
# c.BaseIPythonApplication.profile = u'default'

# Path to an extra config file to load.
#
# If specified, load this config file in addition to any other IPython config.
# c.BaseIPythonApplication.extra_config_file = u''

# Create a massive crash report when IPython encounters what may be an internal
# error.  The default is to append a short message to the usual traceback
# c.BaseIPythonApplication.verbose_crash = False

# The name of the IPython directory. This directory is used for logging
# configuration (through profiles), history storage, etc. The default is usually
# $HOME/.ipython. This option can also be specified through the environment
# variable IPYTHONDIR.
# c.BaseIPythonApplication.ipython_dir = u''

# Whether to install the default config files into the profile dir. If a new
# profile is being created, and IPython contains config files for that profile,
# then they will be staged into the new directory.  Otherwise, default config
# files will be automatically generated.
# c.BaseIPythonApplication.copy_config_files = False

# Whether to create profile dir if it doesn't exist
# c.BaseIPythonApplication.auto_create = False

# Whether to overwrite existing config files when copying
# c.BaseIPythonApplication.overwrite = False

#------------------------------------------------------------------------------
# TerminalIPythonApp configuration
#------------------------------------------------------------------------------

# If a command or file is given via the command-line, e.g. 'ipython foo.py',
# start an interactive shell after executing the file or command.
# c.TerminalIPythonApp.force_interact = False

# Whether to display a banner upon starting IPython.
# c.TerminalIPythonApp.display_banner = True

# Start IPython quickly by skipping the loading of config files.
# c.TerminalIPythonApp.quick = False

#------------------------------------------------------------------------------
# InteractiveShell configuration
#------------------------------------------------------------------------------

# An enhanced, interactive shell for Python.

# Use colors for displaying information about objects. Because this information
# is passed through a pager (like 'less'), and some pagers get confused with
# color codes, this capability can be turned off.
c.InteractiveShell.color_info = True

# A list of ast.NodeTransformer subclass instances, which will be applied to
# user input before code is run.
# c.InteractiveShell.ast_transformers = traitlets.Undefined

#
c.InteractiveShell.history_length = 1000

# Don't call post-execute functions that have failed in the past.
# c.InteractiveShell.disable_failing_post_execute = False

# Show rewritten input, e.g. for autocall.
c.InteractiveShell.show_rewritten_input = True

# Set the color scheme (NoColor, Linux, or LightBG).
c.InteractiveShell.colors = 'Linux'

# If True, anything that would be passed to the pager will be displayed as
# regular output instead.
# c.InteractiveShell.display_page = False

# Autoindent IPython code entered interactively.
c.InteractiveShell.autoindent = True

#
# c.InteractiveShell.separate_in = '\n'

#
# c.InteractiveShell.xmode = 'Context'

# Enable magic commands to be called without the leading %.
# c.InteractiveShell.automagic = True

# Deprecated, use PromptManager.in2_template
# c.InteractiveShell.prompt_in2 = '   .\\D.: '

#
# c.InteractiveShell.separate_out = ''

# Deprecated, use PromptManager.in_template
# c.InteractiveShell.prompt_in1 = 'In [\\#]: '

# **Deprecated**
#
# Enable deep (recursive) reloading by default. IPython can use the deep_reload
# module which reloads changes in modules recursively (it replaces the reload()
# function, so you don't need to change anything to use it). `deep_reload`
# forces a full reload of modules whose code may have changed, which the default
# reload() function does not.  When deep_reload is off, IPython will use the
# normal reload(), but deep_reload will still be available as dreload().
# c.InteractiveShell.deep_reload = False

# The number of saved history entries to be loaded into the readline buffer at
# startup.
# c.InteractiveShell.history_load_length = 1000

#
# c.InteractiveShell.separate_out2 = ''

# Deprecated, use PromptManager.justify
# c.InteractiveShell.prompts_pad_left = True

# The part of the banner to be printed before the profile
# c.InteractiveShell.banner1 = 'Python 2.7.10 (default, Sep  7 2015, 13:51:49) \nType "copyright", "credits" or "license" for more information.\n\nIPython 4.0.0 -- An enhanced Interactive Python.\n?         -> Introduction and overview of IPython\'s features.\n%quickref -> Quick reference.\nhelp      -> Python\'s own help system.\nobject?   -> Details about \'object\', use \'object??\' for extra details.\n'

#
# c.InteractiveShell.readline_parse_and_bind = traitlets.Undefined

# The part of the banner to be printed after the profile
# c.InteractiveShell.banner2 = ''

# Set the size of the output cache.  The default is 1000, you can change it
# permanently in your config file.  Setting it to 0 completely disables the
# caching system, and the minimum value accepted is 20 (if you provide a value
# less than 20, it is reset to 0 and a warning is issued).  This limit is
# defined because otherwise you'll spend more time re-flushing a too small cache
# than working
# c.InteractiveShell.cache_size = 1000

#
# c.InteractiveShell.object_info_string_level = 0

#
# c.InteractiveShell.ipython_dir = ''

#
# c.InteractiveShell.readline_remove_delims = '-/~'

# Start logging to the default log file in overwrite mode. Use `logappend` to
# specify a log file to **append** logs to.
# c.InteractiveShell.logstart = False

# The name of the logfile to use.
# c.InteractiveShell.logfile = ''

#
# c.InteractiveShell.wildcards_case_sensitive = True

# Save multi-line entries as one entry in readline history
# c.InteractiveShell.multiline_history = True

#
# c.InteractiveShell.readline_use = True

# Start logging to the given file in append mode. Use `logfile` to specify a log
# file to **overwrite** logs to.
# c.InteractiveShell.logappend = ''

# Make IPython automatically call any callable object even if you didn't type
# explicit parentheses. For example, 'str 43' becomes 'str(43)' automatically.
# The value can be '0' to disable the feature, '1' for 'smart' autocall, where
# it is not applied if there are no more arguments on the line, and '2' for
# 'full' autocall, where all callable objects are automatically called (even if
# no arguments are present).
# c.InteractiveShell.autocall = 0

#
# c.InteractiveShell.quiet = False

# Deprecated, use PromptManager.out_template
# c.InteractiveShell.prompt_out = 'Out[\\#]: '

#
# c.InteractiveShell.debug = False

# 'all', 'last', 'last_expr' or 'none', specifying which nodes should be run
# interactively (displaying output from expressions).
# c.InteractiveShell.ast_node_interactivity = 'last_expr'

# Automatically call the pdb debugger after every exception.
# c.InteractiveShell.pdb = False

#------------------------------------------------------------------------------
# TerminalInteractiveShell configuration
#------------------------------------------------------------------------------

# auto editing of files with syntax errors.
# c.TerminalInteractiveShell.autoedit_syntax = False

# Number of lines of your screen, used to control printing of very long strings.
# Strings longer than this number of lines will be sent through a pager instead
# of directly printed.  The default value for this is 0, which means IPython
# will auto-detect your screen size every time it needs to print certain
# potentially long strings (this doesn't change the behavior of the 'print'
# keyword, it's only triggered internally). If for some reason this isn't
# working well (it needs curses support), specify it yourself. Otherwise don't
# change the default.
# c.TerminalInteractiveShell.screen_length = 0

# Enable auto setting the terminal title.
# c.TerminalInteractiveShell.term_title = False

# Set to confirm when you try to exit IPython with an EOF (Control-D in Unix,
# Control-Z/Enter in Windows). By typing 'exit' or 'quit', you can force a
# direct exit without any confirmation.
c.TerminalInteractiveShell.confirm_exit = False

# Set the editor used by IPython (default to $EDITOR/vi/notepad).
c.TerminalInteractiveShell.editor = u'vim'

# The shell program to be used for paging.
c.TerminalInteractiveShell.pager = 'less'

#------------------------------------------------------------------------------
# PromptManager configuration
#------------------------------------------------------------------------------

# This is the primary interface for producing IPython's prompts.

# Output prompt. '\#' will be transformed to the prompt number
# c.PromptManager.out_template = 'Out[\\#]: '

# Continuation prompt.
# c.PromptManager.in2_template = '   .\\D.: '

#
# c.PromptManager.color_scheme = 'Linux'

# Input prompt.  '\#' will be transformed to the prompt number
# c.PromptManager.in_template = 'In [\\#]: '

# If True (default), each prompt will be right-aligned with the preceding one.
# c.PromptManager.justify = True

#------------------------------------------------------------------------------
# HistoryAccessorBase configuration
#------------------------------------------------------------------------------

# An abstract class for History Accessors

#------------------------------------------------------------------------------
# HistoryAccessor configuration
#------------------------------------------------------------------------------

# Access the history database without adding to it.
#
# This is intended for use by standalone history tools. IPython shells use
# HistoryManager, below, which is a subclass of this.

# Path to file to use for SQLite history database.
#
# By default, IPython will put the history database in the IPython profile
# directory.  If you would rather share one history among profiles, you can set
# this value in each, so that they are consistent.
#
# Due to an issue with fcntl, SQLite is known to misbehave on some NFS mounts.
# If you see IPython hanging, try setting this to something on a local disk,
# e.g::
#
#     ipython --HistoryManager.hist_file=/tmp/ipython_hist.sqlite
# c.HistoryAccessor.hist_file = u''

# enable the SQLite history
#
# set enabled=False to disable the SQLite history, in which case there will be
# no stored history, no SQLite connection, and no background saving thread.
# This may be necessary in some threaded environments where IPython is embedded.
# c.HistoryAccessor.enabled = True

# Options for configuring the SQLite connection
#
# These options are passed as keyword args to sqlite3.connect when establishing
# database conenctions.
# c.HistoryAccessor.connection_options = traitlets.Undefined

#------------------------------------------------------------------------------
# HistoryManager configuration
#------------------------------------------------------------------------------

# A class to organize all history-related functionality in one place.

# Should the history database include output? (default: no)
# c.HistoryManager.db_log_output = False

# Write to database every x commands (higher values save disk access & power).
# Values of 1 or less effectively disable caching.
# c.HistoryManager.db_cache_size = 0

#------------------------------------------------------------------------------
# LoggingConfigurable configuration
#------------------------------------------------------------------------------

# A parent class for Configurables that log.
#
# Subclasses have a log trait, and the default behavior is to get the logger
# from the currently running Application.

#------------------------------------------------------------------------------
# ProfileDir configuration
#------------------------------------------------------------------------------

# An object to manage the profile directory and its resources.
#
# The profile directory is used by all IPython applications, to manage
# configuration, logging and security.
#
# This object knows how to find, create and manage these directories. This
# should be used by any code that wants to handle profiles.

# Set the profile location directly. This overrides the logic used by the
# `profile` option.
# c.ProfileDir.location = u''

#------------------------------------------------------------------------------
# BaseFormatter configuration
#------------------------------------------------------------------------------

# A base formatter class that is configurable.
#
# This formatter should usually be used as the base class of all formatters. It
# is a traited :class:`Configurable` class and includes an extensible API for
# users to determine how their objects are formatted. The following logic is
# used to find a function to format an given object.
#
# 1. The object is introspected to see if it has a method with the name
#    :attr:`print_method`. If is does, that object is passed to that method
#    for formatting.
# 2. If no print method is found, three internal dictionaries are consulted
#    to find print method: :attr:`singleton_printers`, :attr:`type_printers`
#    and :attr:`deferred_printers`.
#
# Users should use these dictionaries to register functions that will be used to
# compute the format data for their objects (if those objects don't have the
# special print methods). The easiest way of using these dictionaries is through
# the :meth:`for_type` and :meth:`for_type_by_name` methods.
#
# If no function/callable is found to compute the format data, ``None`` is
# returned and this format type is not used.

#
# c.BaseFormatter.type_printers = traitlets.Undefined

#
# c.BaseFormatter.deferred_printers = traitlets.Undefined

#
# c.BaseFormatter.enabled = True

#
# c.BaseFormatter.singleton_printers = traitlets.Undefined

#------------------------------------------------------------------------------
# PlainTextFormatter configuration
#------------------------------------------------------------------------------

# The default pretty-printer.
#
# This uses :mod:`IPython.lib.pretty` to compute the format data of the object.
# If the object cannot be pretty printed, :func:`repr` is used. See the
# documentation of :mod:`IPython.lib.pretty` for details on how to write pretty
# printers.  Here is a simple example::
#
#     def dtype_pprinter(obj, p, cycle):
#         if cycle:
#             return p.text('dtype(...)')
#         if hasattr(obj, 'fields'):
#             if obj.fields is None:
#                 p.text(repr(obj))
#             else:
#                 p.begin_group(7, 'dtype([')
#                 for i, field in enumerate(obj.descr):
#                     if i > 0:
#                         p.text(',')
#                         p.breakable()
#                     p.pretty(field)
#                 p.end_group(7, '])')

#
# c.PlainTextFormatter.newline = '\n'

#
# c.PlainTextFormatter.float_precision = ''

#
# c.PlainTextFormatter.verbose = False

# Truncate large collections (lists, dicts, tuples, sets) to this size.
#
# Set to 0 to disable truncation.
# c.PlainTextFormatter.max_seq_length = 1000

#
# c.PlainTextFormatter.max_width = 79

#
# c.PlainTextFormatter.pprint = True

#------------------------------------------------------------------------------
# Completer configuration
#------------------------------------------------------------------------------

# Activate greedy completion
#
# This will enable completion on elements of lists, results of function calls,
# etc., but can be unsafe because the code is actually evaluated on TAB.
# c.Completer.greedy = False

#------------------------------------------------------------------------------
# IPCompleter configuration
#------------------------------------------------------------------------------

# Extension of the completer class with IPython-specific features

# Instruct the completer to omit private method names
#
# Specifically, when completing on ``object.<tab>``.
#
# When 2 [default]: all names that start with '_' will be excluded.
#
# When 1: all 'magic' names (``__foo__``) will be excluded.
#
# When 0: nothing will be excluded.
c.IPCompleter.omit__names = 0

# Whether to merge completion results into a single list
#
# If False, only the completion results from the first non-empty completer will
# be returned.
# c.IPCompleter.merge_completions = True

# Instruct the completer to use __all__ for the completion
#
# Specifically, when completing on ``object.<tab>``.
#
# When True: only those names in obj.__all__ will be included.
#
# When False [default]: the __all__ attribute is ignored
# c.IPCompleter.limit_to__all__ = False

#------------------------------------------------------------------------------
# Magics configuration
#------------------------------------------------------------------------------

# Base class for implementing magic functions.
#
# Shell functions which can be reached as %function_name. All magic functions
# should accept a string, which they can parse for their own needs. This can
# make some functions easier to type, eg `%cd ../` vs. `%cd("../")`
#
# Classes providing magic functions need to subclass this class, and they MUST:
#
# - Use the method decorators `@line_magic` and `@cell_magic` to decorate
#   individual methods as magic functions, AND
#
# - Use the class decorator `@magics_class` to ensure that the magic
#   methods are properly registered at the instance level upon instance
#   initialization.
#
# See :mod:`magic_functions` for examples of actual implementation classes.

#------------------------------------------------------------------------------
# ScriptMagics configuration
#------------------------------------------------------------------------------

# Magics for talking to scripts
#
# This defines a base `%%script` cell magic for running a cell with a program in
# a subprocess, and registers a few top-level magics that call %%script with
# common interpreters.

# Extra script cell magics to define
#
# This generates simple wrappers of `%%script foo` as `%%foo`.
#
# If you want to add script magics that aren't on your path, specify them in
# script_paths
# c.ScriptMagics.script_magics = traitlets.Undefined

# Dict mapping short 'ruby' names to full paths, such as '/opt/secret/bin/ruby'
#
# Only necessary for items in script_magics where the default path will not find
# the right interpreter.
# c.ScriptMagics.script_paths = traitlets.Undefined

#------------------------------------------------------------------------------
# StoreMagics configuration
#------------------------------------------------------------------------------

# Lightweight persistence for python variables.
#
# Provides the %store magic.

# If True, any %store-d variables will be automatically restored when IPython
# starts.
# c.StoreMagics.autorestore = False
