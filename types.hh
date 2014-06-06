#ifndef MIGHTY_TYPES_HH
#define MIGHTY_TYPES_HH

#include <cstdint>
#include <climits>
#include <cstddef>
#include <stdexcept>
#include <string>

namespace mighty
{

enum class language_type
	: std::uint8_t
{
	root,
	empty,
	null,
	terminal,
	repeat,
	alternate,
	catenate
};

static inline
auto print_name (language_type t)
{
	switch (t)
	{
	case language_type::root: return "root";
	case language_type::empty: return "empty";
	case language_type::null: return "null";
	case language_type::terminal: return "terminal";
	case language_type::repeat: return "repeat";
	case language_type::alternate: return "alternate";
	case language_type::catenate: return "catenate";
	}
}


//// Basic language definitions

struct language
{
	const language_type type;

	language () = delete;
	language (language_type t) noexcept
		: type {t}
	{
	}
};

static inline
auto type (const language* L) noexcept
{
	return L->type;
}


//// Mixin definitions

struct derive_recursive
{
	// TODO
};

// Declare here for delta-recursive
struct root_language
	: language
{
	root_language ()
		: language (language_type::root)
	{
	}
};

extern inline
const root_language* make_root ()
{
	static const root_language root;
	return &root;
}

struct delta_recursive
{
	enum class continuation
		: std::uint8_t
	{
		start,
		other_left,
		other_right,
		combine
	};

	// TODO - See how the algorithm stand up under multithreading
	mutable bool cache = false;
	mutable bool fixed = false;
	mutable continuation next = continuation::start;
	mutable const language* visitor = make_root ();
};


//// Concrete language definitions

struct empty_language
	: language
{
	empty_language ()
		: language (language_type::empty)
	{
	}
};

struct null_language
	: language
{
	null_language ()
		: language (language_type::null)
	{
	}
};

struct terminal_language
	: language
{
	// TODO - static typing
	const void* const terminal;

	terminal_language (const void* const terminal)
		: language (language_type::terminal),
		  terminal {terminal}
	{
	}
};

struct repeat_language
	: language
{
	const language* const sublanguage;

	repeat_language (const language* const sublanguage)
		: language (language_type::repeat),
		  sublanguage {sublanguage}
	{
	}
};

struct birecursive_language
	: language
{
	const language* const left;
	const language* const right;

	birecursive_language (language_type type,
	                      const language* left,
	                      const language* right) noexcept
		: language (type),
		  left {left},
		  right {right}
	{
	}
};

struct alternate_language
	: birecursive_language,
	  derive_recursive,
	  delta_recursive
{
	alternate_language (const language* left,
	                    const language* right)
		: birecursive_language (language_type::alternate,
		                        left, right)
	{
	}
};

struct catenate_language
	: birecursive_language,
	  derive_recursive,
	  delta_recursive
{
	catenate_language (const language* left,
	                    const language* right)
		: birecursive_language (language_type::catenate,
		                        left, right)
	{
	}
};

struct any_language
{
	union
	{
		language L;
		empty_language _empty;
		null_language _null;
		terminal_language _terminal;
		repeat_language _repeat;
		alternate_language _alternate;
		catenate_language _catenate;
	};
};


//// Language constructors

extern inline
const empty_language* make_empty ()
{
	static const empty_language empty;
	return &empty;
}

extern inline
const null_language* make_null ()
{
	static const null_language null;
	return &null;
}

static inline
terminal_language* make_terminal (any_language* place,
                                  const void* terminal)
{
	return new (&place->_terminal) terminal_language (terminal);
}

static inline
const repeat_language* make_repeat (any_language* place,
                                    const language* L)
{
	return new (&place->_repeat) repeat_language (L);
}

static inline
const alternate_language* make_alternate (any_language* place,
                                          const language* left,
                                          const language* right)
{
	return new (&place->_alternate) alternate_language (left, right);
}



class bad_cast
	: public std::bad_cast
{
	const std::string what_str;

protected:
	static
	std::string make_what (const char* expected_type,
	                       const language* datum)
	{
		static_assert (CHAR_BIT == 8, "Who the hell uses non--eight-bit bytes?");
		static constexpr
		const auto addrchars = 2 * sizeof (void*);
		char addrbuf [addrchars + 1] = {};
		std::snprintf (addrbuf, addrchars + 1, "%p", datum);

		std::string w;
		w = "Wrong type; expected ";
		w += expected_type;
		w += "; got ";
		w += print_name (type (datum));
		w += " at language 0x";
		w += addrbuf;
		return w;
	}

public:
	bad_cast (const char* expected_type,
	          const language* datum)
		: what_str (make_what (expected_type, datum))
	{
	}

	virtual const char* what () const noexcept override
	{ return what_str.c_str (); }

	virtual ~bad_cast () = default;
};

template <language_type T>
struct downcast_language;

template <>
struct downcast_language <language_type::terminal>
{ auto cast (const language* L) { return static_cast <const terminal_language*> (L); } };

template <>
struct downcast_language <language_type::repeat>
{ auto cast (const language* L) { return static_cast <const repeat_language*> (L); } };

template <>
struct downcast_language <language_type::alternate>
{ auto cast (const language* L) { return static_cast <const alternate_language*> (L); } };

template <>
struct downcast_language <language_type::catenate>
{ auto cast (const language* L) { return static_cast <const catenate_language*> (L); } };

template <language_type T>
static inline
auto downcast (const language* L)
{
	if (type (L) != T)
		throw bad_cast (print_name (T), L);
	return downcast_language <T>::cast (L);
}

} // namespace mighty

#endif
