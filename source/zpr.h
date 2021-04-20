/*
	zpr.h
	Copyright 2020 - 2021, zhiayang

	Licensed under the Apache License, Version 2.0 (the "License");
	you may not use this file except in compliance with the License.
	You may obtain a copy of the License at

	http://www.apache.org/licenses/LICENSE-2.0

	Unless required by applicable law or agreed to in writing, software
	distributed under the License is distributed on an "AS IS" BASIS,
	WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
	See the License for the specific language governing permissions and
	limitations under the License.



	detail::print_floating and detail::print_exponent are adapted from _ftoa and _etoa
	from https://github.com/mpaland/printf, which is licensed under the MIT license,
	reproduced below:

	Copyright Marco Paland (info@paland.com), 2014-2019, PALANDesign Hannover, Germany

	Permission is hereby granted, free of charge, to any person obtaining a copy
	of this software and associated documentation files (the "Software"), to deal
	in the Software without restriction, including without limitation the rights
	to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
	copies of the Software, and to permit persons to whom the Software is
	furnished to do so, subject to the following conditions:

	The above copyright notice and this permission notice shall be included in
	all copies or substantial portions of the Software.

	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
	IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
	FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
	AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
	LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
	OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
	THE SOFTWARE.
*/


/*
	Version 2.1.12
	==============



	Documentation
	=============

	This printing library functions as a lightweight alternative to
	std::format in C++20 (or fmtlib), with the following (non)features:

	1. no exceptions
	2. short and simple implementation without tons of templates
	3. no support for positional arguments (for now)

	Otherwise, it should support most of the "typical use-case" features that std::format provides. In essence it is a type-safe
	alternative to printf supporting custom type printers. The usage is as such:

	zpr::println("{<spec>}", argument);

	where `<spec>` is exactly a `printf`-style format specifier (note: there is no leading colon unlike the fmtlib/python style),
	and where the final type specifier (eg. `s`, `d`) is optional. Floating point values will print as if `g` was used. Size
	specifiers (eg. `lld`) are not supported. Variable width and precision specifiers (eg. `%.*s`) are not supported.

	The currently supported builtin formatters are:
	- integral types            (signed/unsigned char/short/int/long/long long) (but not 'char')
	- floating point types      (float, double, long double)
	- strings                   (char*, const char*, anything container value_type == 'char')
	- booleans                  (prints as 'true'/'false')
	- void*, const void*        (prints with %p)
	- chars                     (char)
	- enums                     (will print as their integer representation)
	- std::pair                 (prints as "{ first, second }")
	- all iterable containers   (with begin(x) and end(x) available -- prints as "[ a, b, ..., c ]")

	For non-constant widths and precisions, there are 3 functions: w(int), p(int), and wp(int, int). these
	functions return a function object, which you should then call with the actual argument value. usage:

	old: zpr::println("{.*}", 3, M_PI);
	new: zpr::println("{}", zpr::p(3)(M_PI));

	for wp(), the width is the first argument, and the precision the second argument. Note that you
	could use this everywhere and not put the widths in the format string at all.

	There are optional #define macros to control behaviour; defining them without a value constitues
	a TRUE -- for FALSE, either `#undef` them, or explicitly define them as 0. For values that are TRUE
	by default, you *must* explicitly `#define` them as 0.

	- ZPR_USE_STD
		this is *TRUE* by default. controls whether or not STL type interfaces are used; with it,
		you get the std::pair printer, and the sprint() overload that returns std::string. that's
		about it. iterator-based container printing is not affected by this flag.

	- ZPR_HEX_0X_RESPECTS_UPPERCASE
		this is *FALSE* by default. basically, if you use '{X}', this setting determines whether
		you'll get '0xDEADBEEF' or '0XDEADBEEF'. i think the capital 'X' looks ugly as heck, so this
		is OFF by default.

	- ZPR_DECIMAL_LOOKUP_TABLE
		this is *TRUE* by default. controls whether we use a lookup table to increase the speed of
		decimal printing. this uses 201 bytes.

	- ZPR_HEXADECIMAL_LOOKUP_TABLE
		this is *TRUE* by default. controls whether we use a lookup table to increase the speed of
		hex printing. this uses 1025 bytes.

	- ZPR_FREESTANDING
		this is *FALSE by default; controls whether or not a standard library implementation is
		available. if not, then the following changes are made:
		(a) the print() family of functions that would print to stdout, and the fprint() family that
			would print to a FILE* are no longer available.

		(b) memset(), memcpy(), memmove(), strlen(), and strncpy() are forward declared according to
			the C library specifications, but they are not defined. they are expected to be defined
			elsewhere in the program.


	Custom Formatters
	-----------------

	To format custom types, specialise the print_formatter struct. An example of how it should be done can be seen from
	the builtin formatters, taking note to follow the signatures.

	A key point to note is that you can specialise both for the 'raw' type -- ie. you can specialise for T&, and also
	for T; the non-decayed specialisation is preferred if both exist.

	(NB: the reason for this is so that we can support reference to array of char, ie. char (&)[N])



	Function List
	-------------

	the type 'tt::str_view' is an simplified version of std::string_view, and has
	implicit constructors from 'const char*' as well as const char (&)[N] (which means we don't
	call strlen() on string literals).

	for user-defined string-like types, feel free to implement operator tt::str_view() for
	an implicit conversion to be able to use those as a format string.

	* print to a callback function, given by the templated parameter 'callback'.
	* it should be a function object defining "operator() (const char*, size_t)",
	* and should print the string pointed to by the first argument, with length
	* specified by the second argument.
	size_t cprint(const CallbackFn& callback, tt::str_view fmt, Args&&... args);

	* print to a buffer, given by the pointer 'buf', with maximum size 'len'.
	* no NULL terminator will be appended to the buffer, including in the situation
	* where the buffer is maximally filled; thus you might want to reserve space for a NULL
	* byte if necessary.
	size_t sprint(char* buf, size_t len, tt::str_view fmt, Args&&... args);

	* only available if ZPR_USE_STD == 1: print to a std::string
	std::string sprint(tt::str_view fmt, Args&&... args);

	* only available if ZPR_FREESTANDING != 0: print to stdout.
	size_t print(tt::str_view fmt, Args&&... args);

	* only available if ZPR_FREESTANDING != 0: print to stdout, followed by a newline '\n'.
	size_t println(tt::str_view fmt, Args&&... args);

	* only available if ZPR_FREESTANDING != 0: print to the specified FILE*
	size_t fprint(FILE* file, tt::str_view fmt, Args&&... args);

	* only available if ZPR_FREESTANDING != 0: print to the specified FILE*, followed by a newline '\n'.
	size_t fprintln(FILE* file, tt::str_view fmt, Args&&... args);










	Version History
	===============

	2.1.12 - 15/03/2021
	-------------------
	Bug fixes:
	- fix handling of control macros when they are defined without a value. Now they will not generate errors, and are
		treated as TRUE. Also update the documentation about this.



	2.1.11 - 15/03/2021
	-------------------
	Bug fixes:
	- fix broken drop() and take_last() for str_view... again



	2.1.10 - 02/01/2021
	-------------------
	Bug fixes:
	- fix truncated `inf` and `nan` when precision is specified.



	2.1.9 - 23/12/2020
	------------------
	Bug fixes:
	- fix unused variable warning when lookup tables were disabled
	- fix pointless assertion in integer printing (`sizeof(T) <= 64` -> `sizeof(T) <= 8`)



	2.1.8 - 14/12/2020
	------------------
	Bug fixes:
	- fix incorrect drop(), drop_last(), take(), and take_last() on tt::str_view



	2.1.7 - 21/11/2020
	------------------
	Bug fixes:
	- fix warning on member initialisation order for cprint



	2.1.6 - 20/11/2020
	------------------
	Bug fixes:
	- fix broken std::pair printing



	2.1.5 - 17/11/2020
	------------------
	Bug fixes:
	- fix broken binary integer printing



	2.1.4 - 13/11/2020
	------------------
	Bug fixes:
	- fix 'comparison between integers of different signs' (or whatever) warning



	2.1.3 - 04/10/2020
	------------------
	Bug fixes:
	- fix an issue preventing user-defined formatters from calling cprint() with the callback given to them



	2.1.2 - 01/10/2020
	------------------
	Bug fixes:
	- fix bool and char not respecting width specifier
	- fix warnings on -Wall -Wextra
	- revert back to memcpy due to alignment concerns



	2.1.1 - 29/09/2020
	------------------
	Remove tt::forward and tt::move to cut down on templates (they're just glorified static_cast<T&&>s anyway)

	Bug fixes:
	- fix potential template failure for signed and unsigned char



	2.1.0 - 28/09/2020
	------------------
	Remove redundant overloads of all the print functions taking const char (&)[N], since tt:str_view
	has an implicit constructor for that.

	Bug fixes:
	- fix the implicit constructor for str_view taking const char (&)[N]



	2.0.2 - 28/09/2020
	------------------
	Small performance improvements. Reduced enable_if usage in favour of template specialisation to improve
	compile times.



	2.0.1 - 28/09/2020
	------------------
	Add `ln` versions of the cprint() functions.



	2.0.0 - 27/09/2020
	------------------
	Completely rewrite the internals; now no longer tuple-based, and we're back to template packs. However,
	instead of using recursion, we are now expanding with fold expressions. This comes with one drawback, however;
	we now do not support in-stream width and precision specifiers (eg. {*.s}). instead, we expose 3 new functions,
	w(), p(), and wp(), which do what their names would suggest; see the documentation above for more info.

	Major version bump due to breaking api change.



	1.6.0 - 26/09/2020
	------------------
	Add the ZPR_FREESTANDING define; see documentation above for details. Also add cprint() and family, which
	allow using a callback of the form `void (*)(char*, size_t)` -- or a compatible template function object,
	to print.

	Bug fixes:
	- fix printing integer zeroes.
	- fix printing hex digits.



	1.5.0 - 10/09/2020
	------------------
	Change the print_formatter interface subtly. Now, you can choose to specialise the template for both the decayed
	type (eg. int, const char*), or for the un-decayed type (eg. const int&, const char (&)[N]). The compiler will
	choose the appropriate specialisation, preferring the latter (not-decayed) type, if it exists.

	Bug fixes:
	- fix issues where we would write NULL bytes into the output stream, due to the 'char(&)[N]' change.
	- fix right-padding and zero-padding behaviour



	1.4.0 - 10/09/2020
	------------------
	Completely remove dependency on STL types. sadly this includes lots of re-implemented type_traits, but an okay
	cost to pay, I suppose. Introduces ZPR_USE_STD define to control this.

	Bug fixes: lots of fixes on formatting correctness; we should now be correct wrt. printf.



	1.3.1 - 09/09/2020
	------------------
	Remove dependency on std::to_chars, to slowly wean off STL. Introduce two new #defines:
	ZPR_DECIMAL_LOOKUP_TABLE, and ZPR_HEXADECIMAL_LOOKUP_TABLE. see docs for info.



	1.3.0 - 08/09/2020
	------------------
	Add overloads for the user-facing print functions that accept std::string_view as the format string. This also
	works for std::string since string_view has an implicit conversion constructor for it.

	Removed user-facing overloads of print and friends that take 'const char*'; now they either take std::string_view
	(as above), or (const char&)[].

	Change the behaviour of string printers; now, any iterable type with a value_type typedef equal to 'char'
	(exactly char -- not signed char, not unsigned char) will print as a string. This lets us cover custom
	string types as well. A side effect of this is that std::vector<char> will print as a string, which might
	be unexpected.

	Bug fixes:
	- broken formatting for floating point numbers
	- '{}' now uses '%g' format for floating point numbers
	- '{p}' (ie. '%p') now works, including '{}' for void* and const void*.



	1.2.0 - 20/07/2020
	------------------
	Use floating-point printer from mpaland/printf, letting us actually beat printf in all cases.



	1.1.1 - 20/07/2020
	------------------
	Don't include unistd.h



	1.1.0 - 20/07/2020
	------------------
	Performance improvements: switched to tuple-based arguments instead of parameter-packed recursion. We are now (slightly)
	faster than printf (as least on two of my systems) as long as no floating-point is involved. (for now we are still forced
	to call snprintf to print floats... charconv pls)

	Bug fixes: fixed broken escaping of {{ and }}.



	1.0.0 - 19/07/2020
	------------------
	Initial release.
*/

#pragma once

#include <cfloat>
#include <cstddef>
#include <cstdint>

#define ZPR_DO_EXPAND(VAL)  VAL ## 1
#define ZPR_EXPAND(VAL)     ZPR_DO_EXPAND(VAL)

// this weird macro soup is necessary to defend against the case where you just do
// #define ZPR_FOO_BAR, but without a value -- we want to treat that as a TRUE.
#if !defined(ZPR_HEX_0X_RESPECTS_UPPERCASE)
	#define ZPR_HEX_0X_RESPECTS_UPPERCASE 0
#elif (ZPR_EXPAND(ZPR_HEX_0X_RESPECTS_UPPERCASE) == 1)
	#undef ZPR_HEX_0X_RESPECTS_UPPERCASE
	#define ZPR_HEX_0X_RESPECTS_UPPERCASE 1
#endif

#if !defined(ZPR_FREESTANDING)
	#define ZPR_FREESTANDING 0
#elif (ZPR_EXPAND(ZPR_FREESTANDING) == 1)
	#undef ZPR_FREESTANDING
	#define ZPR_FREESTANDING 1
#endif

// it needs to be like this so we don't throw a redefinition warning.
#if !defined(ZPR_DECIMAL_LOOKUP_TABLE)
	#define ZPR_DECIMAL_LOOKUP_TABLE 1
#elif (ZPR_EXPAND(ZPR_DECIMAL_LOOKUP_TABLE) == 1)
	#undef ZPR_DECIMAL_LOOKUP_TABLE
	#define ZPR_DECIMAL_LOOKUP_TABLE 1
#endif

#if !defined(ZPR_HEXADECIMAL_LOOKUP_TABLE)
	#define ZPR_HEXADECIMAL_LOOKUP_TABLE 1
#elif (ZPR_EXPAND(ZPR_HEXADECIMAL_LOOKUP_TABLE) == 1)
	#undef ZPR_HEXADECIMAL_LOOKUP_TABLE
	#define ZPR_HEXADECIMAL_LOOKUP_TABLE 1
#endif

#if !defined(ZPR_USE_STD)
	#define ZPR_USE_STD 1
#elif (ZPR_EXPAND(ZPR_USE_STD) == 1)
	#undef ZPR_USE_STD
	#define ZPR_USE_STD 1
#endif


#if !ZPR_FREESTANDING
	#include <cstdio>
	#include <cstring>
#else
	#if defined(ZPR_USE_STD)
		#undef ZPR_USE_STD
	#endif

	#define ZPR_USE_STD 0

	extern "C" void* memset(void* s, int c, size_t n);
	extern "C" void* memcpy(void* dest, const void* src, size_t n);
	extern "C" void* memmove(void* dest, const void* src, size_t n);

	extern "C" size_t strlen(const char* s);
	extern "C" int strncmp(const char* s1, const char* s2, size_t n);

#endif

#if !ZPR_FREESTANDING && ZPR_USE_STD
	#include <string>
	#include <string_view>
	#include <type_traits>
#endif


#undef ZPR_DO_EXPAND
#undef ZPR_EXPAND



namespace zpr::tt
{
#if 0

	using namespace std;

#else
	template <typename T> struct type_identity { using type = T; };

	template <typename T> struct remove_reference      { using type = T; };
	template <typename T> struct remove_reference<T&>  { using type = T; };
	template <typename T> struct remove_reference<T&&> { using type = T; };

	template <typename T, T v>
	struct integral_constant
	{
		static constexpr T value = v;
		typedef T value_type;
		typedef integral_constant type;

		constexpr operator value_type() const { return value; }
		constexpr value_type operator()() const { return value; }
	};

	using true_type = integral_constant<bool, true>;
	using false_type = integral_constant<bool, false>;

	template <typename T> struct remove_cv                   { using type = T; };
	template <typename T> struct remove_cv<const T>          { using type = T; };
	template <typename T> struct remove_cv<volatile T>       { using type = T; };
	template <typename T> struct remove_cv<const volatile T> { using type = T; };

	template <typename T> using remove_cv_t = typename remove_cv<T>::type;

	template <typename> struct is_integral_base : false_type { };

	template <> struct is_integral_base<bool>               : true_type { };
	template <> struct is_integral_base<char>               : true_type { };
	template <> struct is_integral_base<signed char>        : true_type { };
	template <> struct is_integral_base<signed short>       : true_type { };
	template <> struct is_integral_base<signed int>         : true_type { };
	template <> struct is_integral_base<signed long>        : true_type { };
	template <> struct is_integral_base<signed long long>   : true_type { };
	template <> struct is_integral_base<unsigned char>      : true_type { };
	template <> struct is_integral_base<unsigned short>     : true_type { };
	template <> struct is_integral_base<unsigned int>       : true_type { };
	template <> struct is_integral_base<unsigned long>      : true_type { };
	template <> struct is_integral_base<unsigned long long> : true_type { };

	template <typename T> struct is_integral : is_integral_base<remove_cv_t<T>> { };
	template <typename T> constexpr auto is_integral_v = is_integral<T>::value;


	template <typename> struct is_signed_base : false_type { };

	template <> struct is_signed_base<signed char>        : true_type { };
	template <> struct is_signed_base<signed short>       : true_type { };
	template <> struct is_signed_base<signed int>         : true_type { };
	template <> struct is_signed_base<signed long>        : true_type { };
	template <> struct is_signed_base<signed long long>   : true_type { };

	template <typename T> struct is_signed : is_signed_base<remove_cv_t<T>> { };
	template <typename T> constexpr auto is_signed_v = is_signed<T>::value;

	template <typename T, typename U>   struct is_same : false_type { };
	template <typename T>               struct is_same<T, T> : true_type { };

	template <typename A, typename B>
	constexpr auto is_same_v = is_same<A, B>::value;

	// the 3 major compilers -- clang, gcc, and msvc -- support __is_enum. it's not
	// tenable implement is_enum without compiler magic.
	template <typename T> struct is_enum { static constexpr bool value = __is_enum(T); };
	template <typename T> constexpr auto is_enum_v = is_enum<T>::value;

	template <typename T> struct is_reference      : false_type { };
	template <typename T> struct is_reference<T&>  : true_type { };
	template <typename T> struct is_reference<T&&> : true_type { };

	template <typename T> struct is_const          : false_type { };
	template <typename T> struct is_const<const T> : true_type { };

	template <typename T> constexpr auto is_const_v = is_const<T>::value;
	template <typename T> constexpr auto is_reference_v = is_reference<T>::value;

	// a similar story exists for __underlying_type.
	template <typename T> struct underlying_type { using type = __underlying_type(T); };
	template <typename T> using underlying_type_t = typename underlying_type<T>::type;

	template <bool B, typename T = void> struct enable_if { };
	template <typename T> struct enable_if<true, T> { using type = T; };
	template <bool B, typename T = void> using enable_if_t = typename enable_if<B, T>::type;

	template <typename T> struct is_array : false_type { };
	template <typename T> struct is_array<T[]> : true_type { };
	template <typename T, size_t N> struct is_array<T[N]> : true_type { };

	template <typename T> struct remove_extent { using type = T; };
	template <typename T> struct remove_extent<T[]> { using type = T; };
	template <typename T, size_t N> struct remove_extent<T[N]> { using type = T; };

	template <typename T> struct is_function : integral_constant<bool, !is_const_v<const T> && !is_reference_v<T>> { };
	template <typename T> constexpr auto is_function_v = is_function<T>::value;

	template <typename T> auto try_add_pointer(int) -> type_identity<typename remove_reference<T>::type*>;
	template <typename T> auto try_add_pointer(...) -> type_identity<T>;

	template <typename T>
	struct add_pointer : decltype(try_add_pointer<T>(0)) { };

	template <bool B, typename T, typename F> struct conditional { using type = T; };
	template <typename T, typename F> struct conditional<false, T, F> { using type = F; };
	template <bool B, typename T, typename F> using conditional_t = typename conditional<B,T,F>::type;

	template <typename...> struct conjunction : true_type { };
	template <typename B1> struct conjunction<B1> : B1 { };
	template <typename B1, typename... Bn>
	struct conjunction<B1, Bn...> : conditional_t<bool(B1::value), conjunction<Bn...>, B1> { };

	template <typename...> struct disjunction : false_type { };
	template <typename B1> struct disjunction<B1> : B1 { };
	template <typename B1, typename... Bn>
	struct disjunction<B1, Bn...> : conditional_t<bool(B1::value), B1, disjunction<Bn...>>  { };

	template <typename B>
	struct negation : integral_constant<bool, !bool(B::value)> { };

	template <typename T>
	struct decay
	{
	private:
		using U = typename remove_reference<T>::type;
	public:
		using type = typename conditional<
			is_array<U>::value,
			typename remove_extent<U>::type*,
			typename conditional<
				is_function<U>::value,
				typename add_pointer<U>::type,
				typename remove_cv<U>::type
			>::type
		>::type;
	};

	template <typename T> using decay_t = typename decay<T>::type;

	template <typename T, bool = is_integral<T>::value>
	struct _is_unsigned : integral_constant<bool, (T(0) < T(-1))> { };

	template <typename T>
	struct _is_unsigned<T, false> : false_type { };

	template <typename T>
	struct is_unsigned : _is_unsigned<T>::type { };

	template <typename T>
	struct make_unsigned { };

	template <> struct make_unsigned<signed char> { using type = unsigned char; };
	template <> struct make_unsigned<unsigned char> { using type = unsigned char; };
	template <> struct make_unsigned<signed short> { using type = unsigned short; };
	template <> struct make_unsigned<unsigned short> { using type = unsigned short; };
	template <> struct make_unsigned<signed int> { using type = unsigned int; };
	template <> struct make_unsigned<unsigned int> { using type = unsigned int; };
	template <> struct make_unsigned<signed long> { using type = unsigned long; };
	template <> struct make_unsigned<unsigned long> { using type = unsigned long; };
	template <> struct make_unsigned<signed long long> { using type = unsigned long long; };
	template <> struct make_unsigned<unsigned long long> { using type = unsigned long long; };

	template <typename T>
	using make_unsigned_t = typename make_unsigned<T>::type;


	template <typename... Xs>
	using void_t = void;

	template <typename T>
	struct __stop_declval_eval { static constexpr bool __stop = false; };

	template <typename T, typename U = T&&>
	U __declval(int);

	template <typename T>
	T __declval(long);

	template <typename T>
	auto declval() -> decltype(__declval<T>(0))
	{
		static_assert(__stop_declval_eval<T>::__stop, "declval() must not be used!");
		return __stop_declval_eval<T>::__unknown();
	}

	template <typename T> T min(const T& a, const T& b) { return a < b ? a : b; }
	template <typename T> T max(const T& a, const T& b) { return a > b ? a : b; }
	template <typename T> T abs(const T& x) { return x < 0 ? -x : x; }

	template <typename T>
	void swap(T& t1, T& t2)
	{
		T temp = static_cast<T&&>(t1);
		t1 = static_cast<T&&>(t2);
		t2 = static_cast<T&&>(temp);
	}

#endif

	// is_any<X, A, B, ... Z> -> is_same<X, A> || is_same<X, B> || ...
	template <typename T, typename... Ts>
	struct is_any : disjunction<is_same<T, Ts>...> { };

	struct str_view
	{
		str_view() : ptr(nullptr), len(0) { }
		str_view(const char* p, size_t l) : ptr(p), len(l) { }

		template <size_t N>
		str_view(const char (&s)[N]) : ptr(s), len(N - 1) { }

		template <typename T, typename = tt::enable_if_t<tt::is_same_v<const char*, T>>>
		str_view(T s) : ptr(s), len(strlen(s)) { }

	#if ZPR_USE_STD

		str_view(const std::string& str) : ptr(str.data()), len(str.size()) { }
		str_view(const std::string_view& sv) : ptr(sv.data()), len(sv.size()) { }

	#endif

		str_view(str_view&&) = default;
		str_view(const str_view&) = default;
		str_view& operator= (str_view&&) = default;
		str_view& operator= (const str_view&) = default;

		inline bool operator== (const str_view& other) const
		{
			return (this->ptr == other.ptr && this->len == other.len)
				|| (strncmp(this->ptr, other.ptr, tt::min(this->len, other.len)) == 0);
		}

		inline bool operator!= (const str_view& other) const
		{
			return !(*this == other);
		}

		inline const char* begin() const { return this->ptr; }
		inline const char* end() const { return this->ptr + len; }

		inline size_t size() const { return this->len; }
		inline bool empty() const { return this->len == 0; }
		inline const char* data() const { return this->ptr; }

		inline char operator[] (size_t n) { return this->ptr[n]; }

		inline str_view drop(size_t n) const { return (this->size() >= n ? this->substr(n, this->size() - n) : ""); }
		inline str_view take(size_t n) const { return (this->size() >= n ? this->substr(0, n) : *this); }
		inline str_view take_last(size_t n) const { return (this->size() >= n ? this->substr(this->size() - n, n) : *this); }
		inline str_view drop_last(size_t n) const { return (this->size() >= n ? this->substr(0, this->size() - n) : *this); }

		inline str_view& remove_prefix(size_t n) { return (*this = this->drop(n)); }
		inline str_view& remove_suffix(size_t n) { return (*this = this->drop_last(n)); }

	private:
		inline str_view substr(size_t pos, size_t cnt) const { return str_view(this->ptr + pos, cnt); }

		const char* ptr;
		size_t len;
	};
}

namespace zpr
{
	constexpr uint8_t FMT_FLAG_ZERO_PAD         = 0x1;
	constexpr uint8_t FMT_FLAG_ALTERNATE        = 0x2;
	constexpr uint8_t FMT_FLAG_PREPEND_PLUS     = 0x4;
	constexpr uint8_t FMT_FLAG_PREPEND_SPACE    = 0x8;
	constexpr uint8_t FMT_FLAG_HAVE_WIDTH       = 0x10;
	constexpr uint8_t FMT_FLAG_HAVE_PRECISION   = 0x20;
	constexpr uint8_t FMT_FLAG_WIDTH_NEGATIVE   = 0x40;

	struct format_args
	{
		char specifier      = -1;
		uint8_t flags       = 0;

		int64_t width       = -1;
		int64_t length      = -1;
		int64_t precision   = -1;

		bool zero_pad() const       { return this->flags & FMT_FLAG_ZERO_PAD; }
		bool alternate() const      { return this->flags & FMT_FLAG_ALTERNATE; }
		bool have_width() const     { return this->flags & FMT_FLAG_HAVE_WIDTH; }
		bool have_precision() const { return this->flags & FMT_FLAG_HAVE_PRECISION; }
		bool prepend_plus() const   { return this->flags & FMT_FLAG_PREPEND_PLUS; }
		bool prepend_space() const  { return this->flags & FMT_FLAG_PREPEND_SPACE; }

		bool negative_width() const { return have_width() && (this->flags & FMT_FLAG_WIDTH_NEGATIVE); }
		bool positive_width() const { return have_width() && !negative_width(); }

		void set_precision(int64_t p)
		{
			this->precision = p;
			this->flags |= FMT_FLAG_HAVE_PRECISION;
		}

		void set_width(int64_t w)
		{
			this->width = w;
			this->flags |= FMT_FLAG_HAVE_WIDTH;

			if(w < 0)
				this->flags |= FMT_FLAG_WIDTH_NEGATIVE;
		}
	};

	template <typename T, typename = void>
	struct print_formatter { };


	namespace detail
	{
		template <typename T>
		struct __fmtarg_w
		{
			__fmtarg_w(__fmtarg_w&&) = delete;
			__fmtarg_w(const __fmtarg_w&) = delete;
			__fmtarg_w& operator= (__fmtarg_w&&) = delete;
			__fmtarg_w& operator= (const __fmtarg_w&) = delete;

			__fmtarg_w(T&& x, int width) : arg(static_cast<T&&>(x)), width(width) { }

			T arg;
			int width;
		};

		template <typename T>
		struct __fmtarg_p
		{
			__fmtarg_p(__fmtarg_p&&) = delete;
			__fmtarg_p(const __fmtarg_p&) = delete;
			__fmtarg_p& operator= (__fmtarg_p&&) = delete;
			__fmtarg_p& operator= (const __fmtarg_p&) = delete;

			__fmtarg_p(T&& x, int prec) : arg(static_cast<T&&>(x)), prec(prec) { }

			T arg;
			int prec;
		};

		template <typename T>
		struct __fmtarg_wp
		{
			__fmtarg_wp(__fmtarg_wp&&) = delete;
			__fmtarg_wp(const __fmtarg_wp&) = delete;
			__fmtarg_wp& operator= (__fmtarg_wp&&) = delete;
			__fmtarg_wp& operator= (const __fmtarg_wp&) = delete;

			__fmtarg_wp(T&& x, int width, int prec) : arg(static_cast<T&&>(x)), prec(prec), width(width) { }

			T arg;
			int prec;
			int width;
		};

		struct __fmtarg_w_helper
		{
			__fmtarg_w_helper(int w) : width(w) { }

			template <typename T> inline __fmtarg_w<T&&> operator() (T&& val)
			{
				return __fmtarg_w<T&&>(static_cast<T&&>(val), this->width);
			}

			int width;
		};

		struct __fmtarg_p_helper
		{
			__fmtarg_p_helper(int p) : prec(p) { }

			template <typename T> inline __fmtarg_p<T&&> operator() (T&& val)
			{
				return __fmtarg_p<T&&>(static_cast<T&&>(val), this->prec);
			}

			int prec;
		};

		struct __fmtarg_wp_helper
		{
			__fmtarg_wp_helper(int w, int p) : width(w), prec(p) { }

			template <typename T> inline __fmtarg_wp<T&&> operator() (T&& val)
			{
				return __fmtarg_wp<T&&>(static_cast<T&&>(val), this->width, this->prec);
			}

			int width;
			int prec;
		};

		struct dummy_appender
		{
			void operator() (char c);
			void operator() (tt::str_view sv);
			void operator() (char c, size_t n);
			void operator() (const char* begin, const char* end);
			void operator() (const char* begin, size_t len);
		};

		template <typename T, typename = void>
		struct has_formatter : tt::false_type { };

		template <typename T>
		struct has_formatter<T, tt::void_t<decltype(tt::declval<print_formatter<T>>()
			.print(tt::declval<T>(), dummy_appender(), { }))>
		> : tt::true_type { };

		template <typename T>
		constexpr bool has_formatter_v = has_formatter<T>::value;


		template <typename T, typename = void>
		struct is_iterable : tt::false_type { };

		template <typename T>
		struct is_iterable<T, tt::void_t<
			decltype(begin(tt::declval<T&>())),
			decltype(end(tt::declval<T&>()))
		>> : tt::true_type { };

		static inline format_args parse_fmt_spec(tt::str_view sv)
		{
			// remove the first and last (they are { and })
			sv = sv.drop(1).drop_last(1);

			format_args fmt_args = { };
			{
				while(sv.size() > 0)
				{
					switch(sv[0])
					{
						case '0':   fmt_args.flags |= FMT_FLAG_ZERO_PAD; sv.remove_prefix(1); continue;
						case '#':   fmt_args.flags |= FMT_FLAG_ALTERNATE; sv.remove_prefix(1); continue;
						case '-':   fmt_args.flags |= FMT_FLAG_WIDTH_NEGATIVE; sv.remove_prefix(1); continue;
						case '+':   fmt_args.flags |= FMT_FLAG_PREPEND_PLUS; sv.remove_prefix(1); continue;
						case ' ':   fmt_args.flags |= FMT_FLAG_PREPEND_SPACE; sv.remove_prefix(1); continue;
						default:    break;
					}

					break;
				}

				if(sv.empty())
					goto done;

				if('0' <= sv[0] && sv[0] <= '9')
				{
					fmt_args.flags |= FMT_FLAG_HAVE_WIDTH;

					size_t k = 0;
					fmt_args.width = 0;

					while(sv.size() > k && ('0' <= sv[k] && sv[k] <= '9'))
						(fmt_args.width = 10 * fmt_args.width + (sv[k] - '0')), k++;

					sv.remove_prefix(k);
				}

				if(sv.empty())
					goto done;

				if(sv.size() >= 2 && sv[0] == '.')
				{
					sv.remove_prefix(1);

					if(sv[0] == '-')
					{
						// just ignore negative precision i guess.
						size_t k = 1;
						while(sv.size() > k && ('0' <= sv[k] && sv[k] <= '9'))
							k++;

						sv.remove_prefix(k);
					}
					else if('0' <= sv[0] && sv[0] <= '9')
					{
						fmt_args.flags |= FMT_FLAG_HAVE_PRECISION;
						fmt_args.precision = 0;

						size_t k = 0;
						while(sv.size() > k && ('0' <= sv[k] && sv[k] <= '9'))
							(fmt_args.precision = 10 * fmt_args.precision + (sv[k] - '0')), k++;

						sv.remove_prefix(k);
					}
				}

				if(!sv.empty())
					fmt_args.specifier = sv[0];
			}

		done:
			return fmt_args;
		}

		template <typename CallbackFn>
		size_t print_string(CallbackFn&& cb, const char* str, size_t len, format_args args)
		{
			int64_t string_length = 0;

			if(args.have_precision())   string_length = tt::min(args.precision, static_cast<int64_t>(len));
			else                        string_length = static_cast<int64_t>(len);

			size_t ret = string_length;
			auto padding_width = args.width - string_length;

			if(args.positive_width() && padding_width > 0)
				cb(args.zero_pad() ? '0' : ' ', padding_width), ret += padding_width;

			cb(str, string_length);

			if(args.negative_width() && padding_width > 0)
				cb(args.zero_pad() ? '0' : ' ', padding_width), ret += padding_width;

			return ret;
		}

		template <typename CallbackFn>
		size_t print_special_floating(CallbackFn&& cb, double value, format_args args)
		{
			// uwu. apparently, `inf` and `nan` are never truncated.
			args.set_precision(999);

			if(value != value)
				return print_string(cb, "nan", 3, static_cast<format_args&&>(args));

			if(value < -DBL_MAX)
				return print_string(cb, "-inf", 4, static_cast<format_args&&>(args));

			if(value > DBL_MAX)
			{
				return print_string(cb, args.prepend_plus()
					? "+inf" : args.prepend_space()
					? " inf" : "inf",
					args.prepend_space() || args.prepend_plus() ? 4 : 3,
					static_cast<format_args&&>(args)
				);
			}

			return 0;
		}




		// forward declare these
		template <typename CallbackFn>
		size_t print_floating(CallbackFn&& cb, double value, format_args args);

		template <typename T>
		char* print_decimal_integer(char* buf, size_t bufsz, T value);



		template <typename CallbackFn>
		size_t print_exponent(CallbackFn&& cb, double value, format_args args)
		{
			constexpr int DEFAULT_PRECISION = 6;

			// check for NaN and special values
			if((value != value) || (value > DBL_MAX) || (value < -DBL_MAX))
				return print_special_floating(cb, value, static_cast<format_args&&>(args));

			int prec = (args.have_precision() ? static_cast<int>(args.precision) : DEFAULT_PRECISION);

			bool use_precision  = args.have_precision();
			bool use_zero_pad   = args.zero_pad() && args.positive_width();
			bool use_right_pad  = !use_zero_pad && args.negative_width();
			// bool use_left_pad   = !use_zero_pad && args.positive_width();

			// determine the sign
			const bool negative = (value < 0);
			if(negative)
				value = -value;

			// determine the decimal exponent
			// based on the algorithm by David Gay (https://www.ampl.com/netlib/fp/dtoa.c)
			union {
				uint64_t U;
				double F;
			} conv;

			conv.F = value;
			auto exp2 = static_cast<int64_t>((conv.U >> 52U) & 0x07FFU) - 1023; // effectively log2
			conv.U = (conv.U & ((1ULL << 52U) - 1U)) | (1023ULL << 52U);        // drop the exponent so conv.F is now in [1,2)

			// now approximate log10 from the log2 integer part and an expansion of ln around 1.5
			auto expval = static_cast<int64_t>(0.1760912590558 + exp2 * 0.301029995663981 + (conv.F - 1.5) * 0.289529654602168);

			// now we want to compute 10^expval but we want to be sure it won't overflow
			exp2 = static_cast<int64_t>(expval * 3.321928094887362 + 0.5);

			const double z = expval * 2.302585092994046 - exp2 * 0.6931471805599453;
			const double z2 = z * z;

			conv.U = static_cast<uint64_t>(exp2 + 1023) << 52U;

			// compute exp(z) using continued fractions, see https://en.wikipedia.org/wiki/Exponential_function#Continued_fractions_for_ex
			conv.F *= 1 + 2 * z / (2 - z + (z2 / (6 + (z2 / (10 + z2 / 14)))));

			// correct for rounding errors
			if(value < conv.F)
			{
				expval--;
				conv.F /= 10;
			}

			// the exponent format is "%+02d" and largest value is "307", so set aside 4-5 characters (including the e+ part)
			int minwidth = (-100 < expval && expval < 100) ? 4U : 5U;

			// in "%g" mode, "prec" is the number of *significant figures* not decimals
			if(args.specifier == 'g' || args.specifier == 'G')
			{
				// do we want to fall-back to "%f" mode?
				if((value >= 1e-4) && (value < 1e6))
				{
					if(static_cast<int64_t>(prec) > expval)
						prec = static_cast<uint64_t>(static_cast<int64_t>(prec) - expval - 1);

					else
						prec = 0;

					args.precision = prec;

					// no characters in exponent
					minwidth = 0;
					expval = 0;
				}
				else
				{
					// we use one sigfig for the whole part
					if(prec > 0 && use_precision)
						prec -= 1;
				}
			}

			// will everything fit?
			uint64_t fwidth = args.width;
			if(args.width > minwidth)
			{
				// we didn't fall-back so subtract the characters required for the exponent
				fwidth -= minwidth;
			}
			else
			{
				// not enough characters, so go back to default sizing
				fwidth = 0;
			}

			if(use_right_pad && minwidth)
			{
				// if we're padding on the right, DON'T pad the floating part
				fwidth = 0;
			}

			// rescale the float value
			if(expval)
				value /= conv.F;

			// output the floating part

			auto args_copy = args;
			args_copy.width = fwidth;
			auto len = static_cast<int64_t>(print_floating(cb, negative ? -value : value, args_copy));

			// output the exponent part
			if(minwidth > 0)
			{
				len++;
				if(args.specifier & 0x20)   cb('e');
				else                        cb('E');

				// output the exponent value
				char digits_buf[8] = { };
				size_t digits_len = 0;

				auto buf = print_decimal_integer(digits_buf, 8, static_cast<int64_t>(tt::abs(expval)));
				digits_len = 8 - (buf - digits_buf);

				len += digits_len + 1;
				cb(expval < 0 ? '-' : '+');

				// zero-pad to minwidth - 2
				if(auto tmp = (minwidth - 2) - static_cast<int>(digits_len); tmp > 0)
					len += tmp, cb('0', tmp);

				cb(buf, digits_len);

				// might need to right-pad spaces
				if(use_right_pad && args.width > len)
					cb(' ', args.width - len), len = args.width;
			}

			return len;
		}


		template <typename CallbackFn>
		size_t print_floating(CallbackFn&& cb, double value, format_args args)
		{
			constexpr int DEFAULT_PRECISION = 6;
			constexpr size_t MAX_BUFFER_LEN = 128;
			constexpr long double EXPONENTIAL_CUTOFF = 1e15;

			char buf[MAX_BUFFER_LEN] = { 0 };

			size_t len = 0;

			int prec = (args.have_precision() ? static_cast<int>(args.precision) : DEFAULT_PRECISION);

			bool use_zero_pad   = args.zero_pad() && args.positive_width();
			bool use_left_pad   = !use_zero_pad && args.positive_width();
			bool use_right_pad  = !use_zero_pad && args.negative_width();

			// powers of 10
			constexpr double pow10[] = {
				1,
				10,
				100,
				1000,
				10000,
				100000,
				1000000,
				10000000,
				100000000,
				1000000000,
				10000000000,
				100000000000,
				1000000000000,
				10000000000000,
				100000000000000,
				1000000000000000,
				10000000000000000,
			};

			// test for special values
			if((value != value) || (value > DBL_MAX) || (value < -DBL_MAX))
				return print_special_floating(cb, value, static_cast<format_args&&>(args));

			// switch to exponential for large values.
			if((value > EXPONENTIAL_CUTOFF) || (value < -EXPONENTIAL_CUTOFF))
				return print_exponent(cb, value, static_cast<format_args&&>(args));

			// default to g.
			if(args.specifier == -1)
				args.specifier = 'g';

			// test for negative
			const bool negative = (value < 0);
			if(value < 0)
				value = -value;

			// limit precision to 16, cause a prec >= 17 can lead to overflow errors
			while((len < MAX_BUFFER_LEN) && (prec > 16))
			{
				buf[len++] = '0';
				prec--;
			}

			auto whole = static_cast<int64_t>(value);
			auto tmp = (value - whole) * pow10[prec];
			auto frac = static_cast<unsigned long>(tmp);

			double diff = tmp - frac;

			if(diff > 0.5)
			{
				frac += 1;

				// handle rollover, e.g. case 0.99 with prec 1 is 1.0
				if(frac >= pow10[prec])
				{
					frac = 0;
					whole += 1;
				}
			}
			else if(diff < 0.5)
			{
				// ?
			}
			else if((frac == 0U) || (frac & 1U))
			{
				// if halfway, round up if odd OR if last digit is 0
				frac += 1;
			}

			if(prec == 0U)
			{
				diff = value - static_cast<double>(whole);
				if((!(diff < 0.5) || (diff > 0.5)) && (whole & 1))
				{
					// exactly 0.5 and ODD, then round up
					// 1.5 -> 2, but 2.5 -> 2
					whole += 1;
				}
			}
			else
			{
				auto count = prec;

				bool flag = (args.specifier == 'g' || args.specifier == 'G');
				// now do fractional part, as an unsigned number
				while(len < MAX_BUFFER_LEN)
				{
					if(flag && (frac % 10) == 0)
						goto skip;

					flag = false;
					buf[len++] = static_cast<char>('0' + (frac % 10));

				skip:
					count -= 1;
					if(!(frac /= 10))
						break;
				}

				// add extra 0s
				while((len < MAX_BUFFER_LEN) && (count-- > 0))
					buf[len++] = '0';

				// add decimal
				if(len < MAX_BUFFER_LEN)
					buf[len++] = '.';
			}

			// do whole part, number is reversed
			while(len < MAX_BUFFER_LEN)
			{
				buf[len++] = static_cast<char>('0' + (whole % 10));
				if(!(whole /= 10))
					break;
			}

			// pad leading zeros
			if(use_zero_pad)
			{
				auto width = args.width;

				if(args.have_width() != 0 && (negative || args.prepend_plus() || args.prepend_space()))
					width--;

				while((len < static_cast<size_t>(width)) && (len < MAX_BUFFER_LEN))
					buf[len++] = '0';
			}

			if(len < MAX_BUFFER_LEN)
			{
				if(negative)
					buf[len++] = '-';

				else if(args.prepend_plus())
					buf[len++] = '+'; // ignore the space if the '+' exists

				else if(args.prepend_space())
					buf[len++] = ' ';
			}

			// reverse it.
			for(size_t i = 0; i < len / 2; i++)
				tt::swap(buf[i], buf[len - i - 1]);

			auto padding_width = tt::max(int64_t(0), args.width - static_cast<int64_t>(len));

			if(use_left_pad) cb(' ', padding_width);
			if(use_zero_pad) cb('0', padding_width);

			cb(buf, len);

			if(use_right_pad)
				cb(' ', padding_width);

			return len + ((use_left_pad || use_right_pad) ? padding_width : 0);
		}


		template <typename T>
		char* print_hex_integer(char* buf, size_t bufsz, T value)
		{
			static_assert(sizeof(T) <= 8);

		#if ZPR_HEXADECIMAL_LOOKUP_TABLE
			constexpr const char lookup_table[] =
				"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
				"202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f"
				"404142434445464748494a4b4c4d4e4f505152535455565758595a5b5c5d5e5f"
				"606162636465666768696a6b6c6d6e6f707172737475767778797a7b7c7d7e7f"
				"808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f"
				"a0a1a2a3a4a5a6a7a8a9aaabacadaeafb0b1b2b3b4b5b6b7b8b9babbbcbdbebf"
				"c0c1c2c3c4c5c6c7c8c9cacbcccdcecfd0d1d2d3d4d5d6d7d8d9dadbdcdddedf"
				"e0e1e2e3e4e5e6e7e8e9eaebecedeeeff0f1f2f3f4f5f6f7f8f9fafbfcfdfeff";
		#endif

			constexpr auto hex_digit = [](int x) -> char {
				if(0 <= x && x <= 9)
					return '0' + x;

				return 'a' + x - 10;
			};

			char* ptr = buf + bufsz;

			// if we have the lookup table, do two digits at a time.
		#if ZPR_HEXADECIMAL_LOOKUP_TABLE

			constexpr auto copy = [](char* dst, const char* src) {
				memcpy(dst, src, 2);
			};

			while(value >= 0x100)
			{
				copy((ptr -= 2), &lookup_table[(value & 0xFF) * 2]);
				value /= 0x100;
			}

			if(value < 0x10)
				*(--ptr) = hex_digit(value);

			else
				copy((ptr -= 2), &lookup_table[value * 2]);

		#else

			do {
				*(--ptr) = hex_digit(value & 0xF);
				value /= 0x10;

			} while(value > 0);

		#endif

			return ptr;
		}

		template <typename T>
		char* print_binary_integer(char* buf, size_t bufsz, T value)
		{
			char* ptr = buf + bufsz;

			do {
				*(--ptr) = ('0' + (value & 1));
				value >>= 1;

			} while(value > 0);

			return ptr;
		}

		template <typename T>
		char* print_decimal_integer(char* buf, size_t bufsz, T value)
		{
			static_assert(sizeof(T) <= 8);

		#if ZPR_DECIMAL_LOOKUP_TABLE
			constexpr const char lookup_table[] =
				"000102030405060708091011121314151617181920212223242526272829"
				"303132333435363738394041424344454647484950515253545556575859"
				"606162636465666768697071727374757677787980818283848586878889"
				"90919293949596979899";
		#endif

			bool neg = false;
			if constexpr (tt::is_signed_v<T>)
			{
				neg = (value < 0);
				if(neg)
					value = -value;
			}

			char* ptr = buf + bufsz;

			// if we have the lookup table, do two digits at a time.
		#if ZPR_DECIMAL_LOOKUP_TABLE

			constexpr auto copy = [](char* dst, const char* src) {
				memcpy(dst, src, 2);
			};

			while(value >= 100)
			{
				copy((ptr -= 2), &lookup_table[(value % 100) * 2]);
				value /= 100;
			}

			if(value < 10)
				*(--ptr) = (value + '0');

			else
				copy((ptr -= 2), &lookup_table[value * 2]);

		#else

			do {
				*(--ptr) = ('0' + (value % 10));
				value /= 10;

			} while(value > 0);

		#endif

			if(neg)
				*(--ptr) = '-';

			return ptr;
		}

		template <typename T>
		char* print_integer(char* buf, size_t bufsz, T value, int base)
		{
			if(base == 2)       return print_binary_integer(buf, bufsz, value);
			else if(base == 16) return print_hex_integer(buf, bufsz, value);
			else                return print_decimal_integer(buf, bufsz, value);
		}






		struct __print_state_t
		{
			size_t len;
			const char* fmt;
			const char* beg;
			const char* end;
		};

		template <typename CallbackFn, typename T>
		void print_one(CallbackFn&& cb, format_args fmt_args, T&& value)
		{
			using Decayed_T = tt::decay_t<T>;

			static_assert(has_formatter_v<T> || has_formatter_v<Decayed_T>,
				"no formatter for type");

			if constexpr (has_formatter<T>::value)
			{
				print_formatter<T>().print(static_cast<T&&>(value),
					static_cast<CallbackFn&&>(cb), static_cast<format_args&&>(fmt_args));
			}
			else
			{
				print_formatter<Decayed_T>().print(static_cast<T&&>(value),
					static_cast<CallbackFn&&>(cb), static_cast<format_args&&>(fmt_args));
			}
		}

		template <typename CallbackFn, typename T>
		void skip_fmts(__print_state_t* pst, CallbackFn&& cb, T&& value)
		{
			while((static_cast<size_t>(pst->end - pst->fmt) <= pst->len) && pst->end && *pst->end)
			{
				if(*pst->end == '{')
				{
					auto tmp = pst->end;

					// flush whatever we have first:
					cb(pst->beg, pst->end);
					if(pst->end[1] == '{')
					{
						cb('{');
						pst->end += 2;
						pst->beg = pst->end;
						continue;
					}

					while(pst->end[0] && pst->end[0] != '}')
						pst->end++;

					// owo
					if(!pst->end[0])
						return;

					pst->end++;

					auto fmt_spec = parse_fmt_spec(tt::str_view(tmp, pst->end - tmp));
					print_one(static_cast<CallbackFn&&>(cb), static_cast<format_args&&>(fmt_spec), static_cast<T&&>(value));

					pst->beg = pst->end;
					break;
				}
				else if(*pst->end == '}')
				{
					cb(pst->beg, pst->end + 1);

					// well... we don't need to escape }, but for consistency, we accept either } or }} to print one }.
					if(pst->end[1] == '}')
						pst->end++;

					pst->end++;
					pst->beg = pst->end;
				}
				else
				{
					pst->end++;
				}
			}
		}







		template <typename CallbackFn, typename... Args>
		void print(CallbackFn&& cb, tt::str_view sv, Args&&... args)
		{
			__print_state_t st;
			st.len = sv.size();
			st.fmt = sv.data();
			st.beg = sv.data();
			st.end = sv.data();

			(skip_fmts(&st, static_cast<CallbackFn&&>(cb), static_cast<Args&&>(args)), ...);

			// flush
			cb(st.beg, st.len - (st.beg - st.fmt));
		}

		template <size_t N, typename CallbackFn, typename... Args>
		void print(CallbackFn&& cb, const char (&fmt)[N], Args&&... args)
		{
			print(cb, tt::str_view(fmt, N - 1), static_cast<Args&&>(args)...);
		}



	#if ZPR_USE_STD
		struct string_appender
		{
			string_appender(std::string& buf) : buf(buf) { }

			inline void operator() (char c) { this->buf += c; }
			inline void operator() (tt::str_view sv) { this->buf += std::string_view(sv.data(), sv.size()); }
			inline void operator() (char c, size_t n) { this->buf.resize(this->buf.size() + n, c); }
			inline void operator() (const char* begin, const char* end) { this->buf.append(begin, end); }
			inline void operator() (const char* begin, size_t len) { this->buf.append(begin, begin + len); }

			string_appender(string_appender&&) = delete;
			string_appender(const string_appender&) = delete;
			string_appender& operator= (string_appender&&) = delete;
			string_appender& operator= (const string_appender&) = delete;

		private:
			std::string& buf;
		};
	#endif

	#if !ZPR_FREESTANDING
		template <size_t Limit, bool Newline>
		struct file_appender
		{
			file_appender(FILE* fd, size_t& written) : fd(fd), written(written) { }
			~file_appender() { flush(true); }

			file_appender(file_appender&&) = delete;
			file_appender(const file_appender&) = delete;
			file_appender& operator= (file_appender&&) = delete;
			file_appender& operator= (const file_appender&) = delete;

			inline void operator() (char c) { *ptr++ = c; flush(); }

			inline void operator() (tt::str_view sv) { (*this)(sv.data(), sv.size()); }
			inline void operator() (const char* begin, const char* end) { (*this)(begin, static_cast<size_t>(end - begin)); }

			inline void operator() (char c, size_t n)
			{
				while(n > 0)
				{
					auto x = tt::min(n, remaining());
					memset(ptr, c, x);
					ptr += x;
					n -= x;
					flush();
				}
			}

			inline void operator() (const char* begin, size_t len)
			{
				while(len > 0)
				{
					auto x = tt::min(len, remaining());
					memcpy(ptr, begin, x);
					ptr += x;
					begin += x;
					len -= x;

					flush();
				}
			}

		private:
			inline size_t remaining()
			{
				return Limit - (ptr - buf);
			}

			inline void flush(bool last = false)
			{
				if(!last && static_cast<size_t>(ptr - buf) < Limit)
					return;

				fwrite(buf, sizeof(char), ptr - buf, fd);
				written += ptr - buf;

				if(last && Newline)
					written++, fputc('\n', fd);

				ptr = buf;
			}

			FILE* fd = 0;

			char buf[Limit];
			char* ptr = &buf[0];
			size_t& written;
		};
	#endif

		template <typename Fn>
		struct callback_appender
		{
			callback_appender(Fn* callback, bool newline) : len(0), newline(newline), callback(callback) { }
			~callback_appender() { if(newline) { (*callback)("\n", 1); } }

			inline void operator() (char c) { (*callback)(&c, 1); this->len += 1; }
			inline void operator() (tt::str_view sv) { (*callback)(sv.data(), sv.size()); this->len += sv.size(); }
			inline void operator() (const char* begin, const char* end) { (*callback)(begin, end - begin); this->len += (end - begin); }
			inline void operator() (const char* begin, size_t len) { (*callback)(begin, len); this->len += len; }
			inline void operator() (char c, size_t n)
			{
				for(size_t i = 0; i < n; i++)
					(*callback)(&c, 1);

				this->len += n;
			}

			callback_appender(callback_appender&&) = delete;
			callback_appender(const callback_appender&) = delete;
			callback_appender& operator= (callback_appender&&) = delete;
			callback_appender& operator= (const callback_appender&) = delete;

			size_t size() { return this->len; }

		private:
			size_t len;
			bool newline;
			Fn* callback;
		};



		struct buffer_appender
		{
			buffer_appender(char* buf, size_t cap) : buf(buf), cap(cap), len(0) { }

			inline void operator() (char c)
			{
				if(this->len < this->cap)
					this->buf[this->len++] = c;
			}

			inline void operator() (tt::str_view sv)
			{
				auto l = this->remaining(sv.size());
				memmove(&this->buf[this->len], sv.data(), l);
				this->len += l;
			}

			inline void operator() (char c, size_t n)
			{
				for(size_t i = 0; i < this->remaining(n); i++)
					this->buf[this->len++] = c;
			}

			inline void operator() (const char* begin, const char* end)
			{
				(*this)(tt::str_view(begin, end - begin));
			}

			inline void operator() (const char* begin, size_t len)
			{
				(*this)(tt::str_view(begin, len));
			}

			buffer_appender(buffer_appender&&) = delete;
			buffer_appender(const buffer_appender&) = delete;
			buffer_appender& operator= (buffer_appender&&) = delete;
			buffer_appender& operator= (const buffer_appender&) = delete;

			inline size_t size() { return this->len; }

		private:
			inline size_t remaining(size_t n) { return tt::min(this->cap - this->len, n); }

			char* buf = 0;
			size_t cap = 0;
			size_t len = 0;
		};

		constexpr size_t STDIO_BUFFER_SIZE = 4096;
	}




	template <typename CallbackFn, typename... Args>
	size_t cprint(CallbackFn&& callback, tt::str_view fmt, Args&&... args)
	{
		auto appender = detail::callback_appender(&callback, /* newline: */ false);
		detail::print(appender, fmt,
			static_cast<Args&&>(args)...);

		return appender.size();
	}

	template <typename CallbackFn, typename... Args>
	size_t cprintln(CallbackFn&& callback, tt::str_view fmt, Args&&... args)
	{
		auto appender = detail::callback_appender(&callback, /* newline: */ true);
		detail::print(appender, fmt,
			static_cast<Args&&>(args)...);

		return appender.size();
	}

	template <typename... Args>
	size_t sprint(char* buf, size_t len, tt::str_view fmt, Args&&... args)
	{
		auto appender = detail::buffer_appender(buf, len);
		detail::print(appender, fmt,
			static_cast<Args&&>(args)...);

		return appender.size();
	}



// if we are freestanding, we don't have stdio, so we can't print to "stdout".
#if !ZPR_FREESTANDING

	template <typename... Args>
	size_t print(tt::str_view fmt, Args&&... args)
	{
		size_t ret = 0;
		detail::print(detail::file_appender<detail::STDIO_BUFFER_SIZE, false>(stdout, ret), fmt,
			static_cast<Args&&>(args)...);

		return ret;
	}

	template <typename... Args>
	size_t println(tt::str_view fmt, Args&&... args)
	{
		size_t ret = 0;
		detail::print(detail::file_appender<detail::STDIO_BUFFER_SIZE, true>(stdout, ret), fmt,
			static_cast<Args&&>(args)...);

		return ret;
	}

	template <typename... Args>
	size_t fprint(FILE* file, tt::str_view fmt, Args&&... args)
	{
		size_t ret = 0;
		detail::print(detail::file_appender<detail::STDIO_BUFFER_SIZE, false>(file, ret), fmt,
			static_cast<Args&&>(args)...);

		return ret;
	}

	template <typename... Args>
	size_t fprintln(FILE* file, tt::str_view fmt, Args&&... args)
	{
		size_t ret = 0;
		detail::print(detail::file_appender<detail::STDIO_BUFFER_SIZE, true>(file, ret), fmt,
			static_cast<Args&&>(args)...);

		return ret;
	}
#endif













	inline detail::__fmtarg_w_helper w(int width)
	{
		return detail::__fmtarg_w_helper(width);
	}

	inline detail::__fmtarg_p_helper p(int prec)
	{
		return detail::__fmtarg_p_helper(prec);
	}

	inline detail::__fmtarg_wp_helper wp(int width, int prec)
	{
		return detail::__fmtarg_wp_helper(width, prec);
	}


	// formatters lie here.
	template <typename T>
	struct print_formatter<detail::__fmtarg_w<T>>
	{
		template <typename Cb, typename F = detail::__fmtarg_w<T>>
		void print(F&& x, Cb&& cb, format_args args)
		{
			args.set_width(x.width);
			detail::print_one(static_cast<Cb&&>(cb), static_cast<format_args&&>(args), static_cast<T&&>(x.arg));
		}
	};

	template <typename T>
	struct print_formatter<detail::__fmtarg_p<T>>
	{
		template <typename Cb, typename F = detail::__fmtarg_p<T>>
		void print(F&& x, Cb&& cb, format_args args)
		{
			args.set_precision(x.prec);
			detail::print_one(static_cast<Cb&&>(cb), static_cast<format_args&&>(args), static_cast<T&&>(x.arg));
		}
	};

	template <typename T>
	struct print_formatter<detail::__fmtarg_wp<T>>
	{
		template <typename Cb, typename F = detail::__fmtarg_wp<T>>
		void print(F&& x, Cb&& cb, format_args args)
		{
			args.set_width(x.width);
			args.set_precision(x.prec);
			detail::print_one(static_cast<Cb&&>(cb), static_cast<format_args&&>(args), static_cast<T&&>(x.arg));
		}
	};

	namespace detail
	{
		template <typename T>
		struct __int_formatter
		{
			using Decayed_T = tt::decay_t<T>;

			template <typename Cb>
			void print(T x, Cb&& cb, format_args args)
			{
				int base = 10;
				if((args.specifier | 0x20) == 'x')  base = 16;
				else if(args.specifier == 'b')      base = 2;
				else if(args.specifier == 'p')
				{
					base = 16;
					args.specifier = 'x';
					args.flags |= FMT_FLAG_ALTERNATE;
				}

				// if we print base 2 we need 64 digits!
				constexpr size_t digits_buf_sz = 65;
				char digits_buf[digits_buf_sz] = { };

				char* digits = 0;
				size_t digits_len = 0;


				{
					if constexpr (tt::is_unsigned<Decayed_T>::value)
					{
						digits = detail::print_integer(digits_buf, digits_buf_sz, x, base);
						digits_len = digits_buf_sz - (digits - digits_buf);
					}
					else
					{
						if(base == 16)
						{
							digits = detail::print_integer(digits_buf, digits_buf_sz,
								static_cast<tt::make_unsigned_t<Decayed_T>>(x), base);

							digits_len = digits_buf_sz - (digits - digits_buf);
						}
						else
						{
							auto abs_val = tt::abs(x);
							digits = detail::print_integer(digits_buf, digits_buf_sz, abs_val, base);

							digits_len = digits_buf_sz - (digits - digits_buf);
						}
					}

					if('A' <= args.specifier && args.specifier <= 'Z')
						for(size_t i = 0; i < digits_len; i++)
							digits[i] = static_cast<char>(digits[i] - 0x20);
				}

				char prefix[4] = { 0 };
				int64_t prefix_len = 0;
				int64_t prefix_digits_length = 0;
				{
					char* pf = prefix;
					if(args.prepend_plus())
						prefix_len++, *pf++ = '+';

					else if(args.prepend_space())
						prefix_len++, *pf++ = ' ';

					else if(x < 0 && base == 10)
						prefix_len++, *pf++ = '-';

					if(base != 10 && args.alternate())
					{
						*pf++ = '0';
						*pf++ = (ZPR_HEX_0X_RESPECTS_UPPERCASE ? args.specifier : (args.specifier | 0x20));

						prefix_digits_length += 2;
						prefix_len += 2;
					}
				}

				int64_t output_length_with_precision = (args.have_precision()
					? tt::max(args.precision, static_cast<int64_t>(digits_len))
					: static_cast<int64_t>(digits_len)
				);

				int64_t total_digits_length = prefix_digits_length + static_cast<int64_t>(digits_len);
				int64_t normal_length = prefix_len + static_cast<int64_t>(digits_len);
				int64_t length_with_precision = prefix_len + output_length_with_precision;

				bool use_precision = args.have_precision();
				bool use_zero_pad  = args.zero_pad() && args.positive_width() && !use_precision;
				bool use_left_pad  = !use_zero_pad && args.positive_width();
				bool use_right_pad = !use_zero_pad && args.negative_width();

				int64_t padding_width = args.width - length_with_precision;
				int64_t zeropad_width = args.width - normal_length;
				int64_t precpad_width = args.precision - total_digits_length;

				if(padding_width <= 0) { use_left_pad = false; use_right_pad = false; }
				if(zeropad_width <= 0) { use_zero_pad = false; }
				if(precpad_width <= 0) { use_precision = false; }

				// pre-prefix
				if(use_left_pad) cb(' ', padding_width);

				cb(prefix, prefix_len);

				// post-prefix
				if(use_zero_pad) cb('0', zeropad_width);

				// prec-string
				if(use_precision) cb('0', precpad_width);

				cb(digits, digits_len);

				// postfix
				if(use_right_pad) cb(' ', padding_width);
			}
		};
	}

	template <> struct print_formatter<signed char> : detail::__int_formatter<signed char> { };
	template <> struct print_formatter<unsigned char> : detail::__int_formatter<unsigned char> { };
	template <> struct print_formatter<signed short> : detail::__int_formatter<signed short> { };
	template <> struct print_formatter<unsigned short> : detail::__int_formatter<unsigned short> { };
	template <> struct print_formatter<signed int> : detail::__int_formatter<signed int> { };
	template <> struct print_formatter<unsigned int> : detail::__int_formatter<unsigned int> { };
	template <> struct print_formatter<signed long> : detail::__int_formatter<signed long> { };
	template <> struct print_formatter<unsigned long> : detail::__int_formatter<unsigned long> { };
	template <> struct print_formatter<signed long long> : detail::__int_formatter<signed long long> { };
	template <> struct print_formatter<unsigned long long> : detail::__int_formatter<unsigned long long> { };

	template <typename T>
	struct print_formatter<T, typename tt::enable_if<(
		tt::is_enum_v<tt::decay_t<T>>
	)>::type>
	{
		template <typename Cb>
		void print(T x, Cb&& cb, format_args args)
		{
			using underlying = tt::underlying_type_t<tt::decay_t<T>>;
			detail::print_one(static_cast<Cb&&>(cb), static_cast<format_args&&>(args), static_cast<underlying>(x));
		}
	};

	template <>
	struct print_formatter<float>
	{
		template <typename Cb>
		void print(float x, Cb&& cb, format_args args)
		{
			if(args.specifier == 'e' || args.specifier == 'E')
				print_exponent(static_cast<Cb&&>(cb), x, static_cast<format_args&&>(args));

			else
				print_floating(static_cast<Cb&&>(cb), x, static_cast<format_args&&>(args));
		}

		template <typename Cb>
		void print(double x, Cb&& cb, format_args args)
		{
			if(args.specifier == 'e' || args.specifier == 'E')
				print_exponent(static_cast<Cb&&>(cb), x, static_cast<format_args&&>(args));

			else
				print_floating(static_cast<Cb&&>(cb), x, static_cast<format_args&&>(args));
		}
	};

	template <size_t N>
	struct print_formatter<const char (&)[N]>
	{
		template <typename Cb>
		void print(const char (&x)[N], Cb&& cb, format_args args)
		{
			detail::print_string(static_cast<Cb&&>(cb), x, N - 1, static_cast<format_args&&>(args));
		}
	};

	template <>
	struct print_formatter<const char*>
	{
		template <typename Cb>
		void print(const char* x, Cb&& cb, format_args args)
		{
			detail::print_string(static_cast<Cb&&>(cb), x, strlen(x), static_cast<format_args&&>(args));
		}
	};

	template <>
	struct print_formatter<char>
	{
		template <typename Cb>
		void print(char x, Cb&& cb, format_args args)
		{
			detail::print_string(static_cast<Cb&&>(cb), &x, 1, static_cast<format_args&&>(args));
		}
	};

	template <>
	struct print_formatter<bool>
	{
		template <typename Cb>
		void print(bool x, Cb&& cb, format_args args)
		{
			detail::print_string(static_cast<Cb&&>(cb),
				x ? "true" : "false",
				x ? 4      : 5,
				static_cast<format_args&&>(args)
			);
		}
	};

	template <>
	struct print_formatter<const void*>
	{
		template <typename Cb>
		void print(const void* x, Cb&& cb, format_args args)
		{
			args.specifier = 'p';
			print_one(static_cast<Cb&&>(cb), static_cast<format_args&&>(args), reinterpret_cast<uintptr_t>(x));
		}
	};

	template <typename T>
	struct print_formatter<T, typename tt::enable_if<(
		tt::conjunction<detail::is_iterable<T>, tt::is_same<tt::remove_cv_t<typename T::value_type>, char>>::value
	)>::type>
	{
		template <typename Cb>
		void print(const T& x, Cb&& cb, format_args args)
		{
			detail::print_string(static_cast<Cb&&>(cb), x.data(), x.size(), static_cast<format_args&&>(args));
		}
	};

	// exclude strings and string_views
	template <typename T>
	struct print_formatter<T, typename tt::enable_if<(
		tt::conjunction<detail::is_iterable<T>,
			tt::negation<tt::is_same<typename T::value_type, char>>
		>::value
	)>::type>
	{
		template <typename Cb>
		void print(const T& x, Cb&& cb, format_args args)
		{
			if(begin(x) == end(x))
			{
				cb("[ ]");
				return;
			}

			cb("[");
			for(auto it = begin(x);;)
			{
				detail::print_one(static_cast<Cb&&>(cb), args, *it);
				++it;

				if(it != end(x)) cb(", ");
				else             break;
			}

			cb("]");
		}
	};



	template <>
	struct print_formatter<void*> : print_formatter<const void*> { };

	template <>
	struct print_formatter<char*> : print_formatter<const char*> { };

	template <>
	struct print_formatter<double> : print_formatter<float> { };

	template <size_t N>
	struct print_formatter<char (&)[N]> : print_formatter<const char (&)[N]> { };




#if ZPR_USE_STD

	template <typename... Args>
	std::string sprint(tt::str_view fmt, Args&&... args)
	{
		std::string buf;
		detail::print(detail::string_appender(buf), fmt,
			static_cast<Args&&>(args)...);

		return buf;
	}

	template <typename A, typename B>
	struct print_formatter<std::pair<A, B>>
	{
		template <typename Cb>
		void print(const std::pair<A, B>& x, Cb&& cb, format_args args)
		{
			cb("{ ");
			detail::print_one(static_cast<Cb&&>(cb), args, x.first);
			cb(", ");
			detail::print_one(static_cast<Cb&&>(cb), args, x.second);
			cb(" }");
		}
	};
#endif
}
