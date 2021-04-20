/*
	nabs.h
	Copyright 2021, zhiayang

	Licensed under the Apache License, Version 2.0 (the "License");
	you may not use this file except in compliance with the License.
	You may obtain a copy of the License at

	http://www.apache.org/licenses/LICENSE-2.0

	Unless required by applicable law or agreed to in writing, software
	distributed under the License is distributed on an "AS IS" BASIS,
	WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
	See the License for the specific language governing permissions and
	limitations under the License.
*/

/*
	Version 0.1.0
	=============


	Documentation
	=============

	There is none.




	Version History
	===============

	0.1.0 - 20/04/2021
	------------------
	Initial release.
*/

#pragma once

#include "zpr.h"

#include <string>
#include <vector>
#include <filesystem>
#include <type_traits>

#if defined(_WIN32)

#error "windows not supported yet"

#else

#include <stdio.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>

#endif // _WIN32

// standard stuff for everybody
namespace nabs
{
	namespace fs
	{
		using namespace std::filesystem;
	}

	namespace impl
	{
		template <typename... Args>
		[[noreturn]] void error(Args&&... args)
		{
			zpr::fprintln(stderr, "internal error: {}", zpr::sprint(std::forward<Args>(args)...));
			exit(1);
		}
	}

	namespace os
	{
		struct FileOpenFlags
		{
			bool need_write = false;
			bool should_create = false;
			bool append_mode = false;
			bool truncate_mode = false;
			int create_perms = 0664;
		};
	}

	struct CompilerFlags
	{
		std::vector<std::string> options;
		std::vector<fs::path> include_paths;
	};
}




// pipes and commands
namespace nabs
{
	namespace os
	{
		#if defined(_WIN32)

		#else
			using Fd = int;
			static constexpr Fd FD_NONE = -1;

			using Proc = pid_t;
			static constexpr Proc PROC_NONE = -1;

			struct PipeDes
			{
				Fd read_end;
				Fd write_end;
			};

			inline PipeDes make_pipe()
			{
				if(int p[2]; pipe(p) < 0)
				{
					impl::error("Pipeline(): {}", strerror(errno));
				}
				else
				{
					// set the pipes to close on exec, so that we do not have dangling write ends
					// left open in children.
					if(fcntl(p[0], F_SETFD, FD_CLOEXEC) < 0 || fcntl(p[1], F_SETFD, FD_CLOEXEC) < 0)
						impl::error("fcntl(FD_CLOEXEC): {}", strerror(errno));

					return PipeDes {
						.read_end = p[0],
						.write_end = p[1]
					};
				}
			}

			inline Fd open_file(const char* path, FileOpenFlags fof)
			{
				int flags = 0;

				if(fof.need_write)  flags |= O_RDWR;
				else                flags |= O_RDONLY;

				if(fof.should_create)   flags |= O_CREAT;
				if(fof.truncate_mode)   flags |= O_TRUNC;
				else if(fof.append_mode)flags |= O_APPEND;

				int fd = 0;
				if(fof.should_create)
					fd = open(path, flags, fof.create_perms);

				else
					fd = open(path, flags);

				if(fd < 0)
					impl::error("open('{}'): {}", path, strerror(errno));

				return fd;
			}
		#endif // _WIN32
	}

	namespace impl
	{
		inline void split_program(std::vector<os::Fd> fds);

		struct Part;

		struct Pipeline
		{
			bool empty() const { return this->parts.empty(); }

			int run();
			std::vector<os::Proc> runAsync();

			std::vector<Part> parts;

			friend struct Part;

		private:
			std::vector<os::Proc> runAsync(os::Fd in_fd);
		};

		struct Part
		{
			Part(Part&&) = default;
			Part(const Part&) = default;

			Part& operator= (Part&&) = default;
			Part& operator= (const Part&) = default;

			int run(os::Fd in_fd, os::Fd out_fd, os::Fd err_fd);
			os::Proc runAsync(os::Fd in_fd, os::Fd out_fd, os::Fd err_fd);

			char** make_args()
			{
				char** args_array = new char*[this->arguments.size() + 2];
				for(size_t i = 0; i < arguments.size(); i++)
					args_array[1 + i] = const_cast<char*>(this->arguments[i].c_str());

				args_array[0] = const_cast<char*>(this->name.c_str());
				args_array[this->arguments.size() + 1] = nullptr;
				return args_array;
			}

			static constexpr int TYPE_PROC = 1;
			static constexpr int TYPE_FILE = 2;
			static constexpr int TYPE_SPLIT  = 3;

			int type() const  { return this->_type; }
			int flags() const { return this->_flags; }

			// used to check... certain things.
			bool is_passive() const { return this->type() == TYPE_FILE; }

			static Part of_command(std::string name, std::vector<std::string> args = { })
			{
				return Part(TYPE_PROC, 0, std::move(name), std::move(args));
			}

			static Part of_file(std::string name)
			{
				return Part(TYPE_FILE, 0, std::move(name), { });
			}

			static Part of_split(std::vector<impl::Pipeline> splits, std::vector<impl::Pipeline*> ptrs)
			{
				auto ret = Part(TYPE_SPLIT, 0, "split", { });
				ret.split_vals = std::move(splits);
				ret.split_ptrs = std::move(ptrs);
				return ret;
			}

		private:
			Part(int type, int flags, std::string name, std::vector<std::string> args)
				: _type(type)
				, _flags(flags)
				, name(std::move(name))
				, arguments(std::move(args))
			{ }

			int _type;
			int _flags;
			std::string name;
			std::vector<std::string> arguments;

			std::vector<impl::Pipeline> split_vals;
			std::vector<impl::Pipeline*> split_ptrs;

			friend struct Pipeline;
		};

		// ADL should let us find this operator.
		inline Pipeline operator| (Pipeline head, const Pipeline& tail)
		{
			constexpr auto TYPE_FILE = Part::TYPE_FILE;

			// the size needs to be > 1 -- such that if the back of the pipeline is a file,
			// we know that it has to be the 'output' file; we must still allow an 'input' file.
			if(head.parts.size() > 1 && head.parts.back().type() == TYPE_FILE)
				error("file() components cannot appear in the middle of a pipeline");

			// check if we are trying to chain 2 files together.
			if(!head.parts.empty() && !tail.parts.empty()
				&& head.parts.back().type() == TYPE_FILE && tail.parts.front().type() == TYPE_FILE)
				error("file() components cannot appear consecutively in a pipeline");

			head.parts.insert(head.parts.end(), tail.parts.begin(), tail.parts.end());
			return head;
		}


		os::Proc Part::runAsync(os::Fd in_fd, os::Fd out_fd, os::Fd err_fd)
		{
			auto dupe_fd = [](int src, int dst, bool close_src = true) {
				// dup2 closes dst for us.
				if(dup2(src, dst) < 0)
					error("dup2({}, {}): {}", src, dst, strerror(errno));

				if(close_src)
					close(src);
			};

			if(this->type() == TYPE_PROC)
			{
				if(auto child = fork(); child < 0)
				{
					error("fork(): {}", strerror(errno));
				}
				else if(child == 0)
				{
					if(in_fd != os::FD_NONE)
					{
						dupe_fd(in_fd, STDIN_FILENO);
					}
					else
					{
						// TODO: make closing stdin configurable
						// for now, we leave stdin open. this might be a problem later,
						// and we definitely want to make it configurable later on.
					}

					if(out_fd != os::FD_NONE)
						dupe_fd(out_fd, STDOUT_FILENO);

					if(err_fd != os::FD_NONE)
						dupe_fd(err_fd, STDERR_FILENO);

					if(execvp(this->name.c_str(), this->make_args()) < 0)
						error("execvp('{}'): {}", this->name, strerror(errno));

					abort();
				}
				else
				{
					return child;
				}
			}
			else if(this->type() == TYPE_FILE)
			{
				// if the in_fd is not -1, then we need to write to the file.
				os::Fd file = os::open_file(this->name.c_str(), os::FileOpenFlags {
					.need_write = (in_fd != os::FD_NONE),

					// TODO: make a way to specify the file opening flags/modes
					.append_mode = true,    // append by default
					.should_create = true,  // create by default
					.create_perms = 0664
				});

				if(in_fd != os::FD_NONE)
					dupe_fd(file, in_fd, /* close_src: */ false);

				if(out_fd != os::FD_NONE)
					dupe_fd(file, out_fd, /* close_src: */ false);

				close(file);
				return os::PROC_NONE;
			}
			else if(this->type() == TYPE_SPLIT)
			{
				// basically we have to emulate what the `tee` program does.
				if(auto child = fork(); child < 0)
				{
					error("fork(): {}", strerror(errno));
				}
				else if(child == 0)
				{
					if(in_fd == os::FD_NONE)
						error("tee() cannot be the first item in a pipeline");

					std::vector<os::Fd> fds;
					std::vector<os::Proc> children;

					auto iterate_splits = [&fds, &children](Pipeline* pl) {

						assert(pl != nullptr);
						auto [ p_read, p_write ] = os::make_pipe();

						auto cs = pl->runAsync(p_read);
						children.insert(children.end(), cs.begin(), cs.end());

						// if the pipeline's first argument is passive, then we write directly into it.
						// we rely on the pipeline's run() to perform dup2() to change our fd as appropriate.
						if(pl->parts[0].is_passive())
						{
							close(p_write);
							fds.push_back(p_read);
						}
						else
						{
							close(p_read);
							fds.push_back(p_write);
						}
					};

					if(!this->split_ptrs.empty())
					{
						for(auto& sp : this->split_ptrs)
							iterate_splits(sp);
					}
					else
					{
						for(auto& sp : this->split_vals)
							iterate_splits(&sp);
					}


					// it's basically like a normal process
					if(in_fd != os::FD_NONE)
						dupe_fd(in_fd, STDIN_FILENO);

					if(out_fd != os::FD_NONE)
						dupe_fd(out_fd, STDOUT_FILENO);

					if(err_fd != os::FD_NONE)
						dupe_fd(err_fd, STDERR_FILENO);

					split_program(std::move(fds));

					// wait for all the children...
					for(auto c : children)
						waitpid(c, nullptr, 0);

					exit(0);
				}
				else
				{
					return child;
				}
			}
			else
			{
				error("unknown pipeline component type '{}'", this->type());
			}
		}

		int Part::run(os::Fd in_fd, os::Fd out_fd, os::Fd err_fd)
		{
			auto child_pid = this->runAsync(in_fd, out_fd, err_fd);
			if(child_pid == os::PROC_NONE)
				return 0;

			int status = 0;
		 	if(waitpid(child_pid, &status, 0) < 0)
		 		error("waitpid({}): {}", child_pid, strerror(errno));

		 	return WEXITSTATUS(status);
		}

		std::vector<os::Proc> Pipeline::runAsync(os::Fd in_fd)
		{
			std::vector<os::Proc> children;
			os::Fd predecessor_pipe = in_fd;

			if(this->empty())
				error("unexpected empty pipeline");

			size_t start = 0;
			if(!this->parts.empty() && this->parts[0].type() == Part::TYPE_FILE)
			{
				if(this->parts.size() > 1)
				{
					// in this case, we should not have an `in_fd`, since that would conflict
					// with the file.
					if(in_fd != os::FD_NONE)
						error("file() must be the last item in a tee()");

					// open this file in read mode.
					os::Fd file = os::open_file(this->parts[0].name.c_str(), os::FileOpenFlags {
						.need_write = false,
						.should_create = false,
					});

					predecessor_pipe = file;
					start = 1;
				}
				else
				{
					// open this file as write mode.
					// now, we just need to dup2 the file descriptor to the in_fd, and return immediately.
					// by passing in_fd, we get the file to open in write mode.
					this->parts[0].runAsync(in_fd, os::FD_NONE, os::FD_NONE);
					return { };
				}
			}

			for(size_t i = start; i < this->parts.size(); i++)
			{
				bool did_file = false;
				bool is_last = (i == this->parts.size() - 1);

				auto [ pipe_read, pipe_write ] = os::make_pipe();

				// we need some special handling here if the output of this stage goes to a file.
				// at the point of the fork, the parent (us) won't have the file opened, so even if
				// we play with dup2() etc, the child will never be able to see the file. so, we need
				// to scan-ahead and see if the next idiot is a file, and if so we must open it now.
				if(!is_last && this->parts[i + 1].type() == Part::TYPE_FILE)
				{
					// 'run' the file (ie. setup the fds); the input to the file is the write end
					// of *THIS* process's Pipeline (ie. output) -- so writing to it will write to the file,
					// and the 'output' of the file will be the input of the *next* process -- so reading from
					// it reads from the file.
					this->parts[i + 1].runAsync(pipe_write, pipe_read, os::FD_NONE);
					did_file = true;

					// if this is not the last component, abort also.
					if(i + 2 != this->parts.size())
						error("unexpected file ('{}') in the middle of a pipeline", this->parts[i + 1].name);
				}

				if(is_last)
				{
					close(pipe_write);
					pipe_write = os::FD_NONE;
				}

				// someone else should have caught this already.
				assert(this->parts[i].type() != Part::TYPE_FILE);

				// TODO: stderr is not handled here
				// we don't really know how to link them together from proc to proc.
				auto child = this->parts[i].runAsync(predecessor_pipe, pipe_write, os::FD_NONE);
				if(child != os::PROC_NONE)
					children.push_back(child);

				// setup the read end for the next process.
				if(predecessor_pipe != os::FD_NONE)
					close(predecessor_pipe);

				predecessor_pipe = pipe_read;
				if(!is_last)
					close(pipe_write);

				if(did_file)
					i += 1;
			}

			close(predecessor_pipe);
			return children;
		}

		std::vector<os::Proc> Pipeline::runAsync()
		{
			return this->runAsync(os::FD_NONE);
		}

		int Pipeline::run()
		{
			auto children = this->runAsync();

			int status = 0;
			for(auto c : children)
				waitpid(c, &status, 0);

			// just use the last one.
			return WEXITSTATUS(status);
		}

		inline void split_program(std::vector<os::Fd> fds)
		{
			char buf[4096] { };
			while(true)
			{
				auto n = read(STDIN_FILENO, buf, 4096);
				if(n <= 0)
				{
					if(n < 0) zpr::fprintln(stderr, "tee(stdin): read error: {}", strerror(errno));
					break;
				}

				if(write(STDOUT_FILENO, buf, n) < 0)
					zpr::fprintln(stderr, "tee(stdout): write error: {}", strerror(errno));

				for(auto& f : fds)
				{
					if(write(f, buf, n) < 0)
						zpr::fprintln(stderr, "tee({}): write error: {}", f, strerror(errno));
				}
			}

			for(auto& f : fds)
				close(f);
		}
	}

	template <typename... Args>
	inline impl::Pipeline cmd(std::string exe, Args&&... args)
	{
		static_assert(((std::is_convertible_v<Args, std::string>) && ...),
			"arguments to cmd() must be convertible to std::string");

		return impl::Pipeline { .parts = {
			impl::Part::of_command(std::move(exe), std::vector<std::string>{std::forward<Args>(args)...})
		} };
	}

	inline impl::Pipeline cmd(std::string exe, std::vector<std::string> args)
	{
		return impl::Pipeline { .parts = {
			impl::Part::of_command(std::move(exe), std::move(args))
		} };
	}

	inline impl::Pipeline file(fs::path path)
	{
		return impl::Pipeline { .parts = {
			impl::Part::of_file(path.native())
		} };
	}

	template <typename... Args>
	inline impl::Pipeline tee(Args&&... args)
	{
		static_assert(((std::is_convertible_v<Args, fs::path>) && ...),
			"tee() requires std::filesystem::path");

		return impl::Pipeline { .parts = {
			impl::Part::of_split(std::vector<impl::Pipeline> { nabs::file(args)... }, { })
		} };
	}

	namespace impl
	{
		template <typename T>
		inline void split_helper(std::vector<Pipeline>& vals, std::vector<Pipeline*>& ptrs, T&& thing)
		{
			static_assert(std::is_same_v<std::decay_t<T>, Pipeline>, "split() requires pipes");
			static_assert(std::is_rvalue_reference_v<decltype(thing)> || std::is_lvalue_reference_v<decltype(thing)>,
				"split() requires either lvalue or rvalue references as arguments");

			if constexpr (std::is_rvalue_reference_v<decltype(thing)>)
				vals.push_back(std::move(thing));

			else
				ptrs.push_back(&thing);
		}

		template <typename T, typename... Args>
		inline void split_helper(std::vector<Pipeline>& vals, std::vector<Pipeline*>& ptrs, T&& thing, Args&&... things)
		{
			split_helper(vals, ptrs, std::forward<T>(thing));
			split_helper(vals, ptrs, std::forward<Args>(things)...);
		}
	}

	template <typename... Args>
	inline impl::Pipeline split(Args&&... args)
	{
		static_assert(sizeof...(args) > 0, "split() requires at least one argument");

		std::vector<impl::Pipeline> vals;
		std::vector<impl::Pipeline*> ptrs;

		impl::split_helper(vals, ptrs, std::forward<Args>(args)...);

		return impl::Pipeline { .parts = {
			impl::Part::of_split(std::move(vals), std::move(ptrs))
		} };
	}
}



// compiler specifics
namespace nabs
{
	struct Compiler
	{
		fs::path path;
		int kind;
		int lang;

		static constexpr int KIND_GCC_CLANG = 1;
		static constexpr int KIND_MSVC_CL   = 2;

		static constexpr int LANG_C         = 1;
		static constexpr int LANG_CPP       = 2;
	};

	namespace impl
	{
		inline std::vector<fs::path> get_path_variable()
		{
			std::vector<fs::path> ret;
			std::string var = getenv("PATH");

			while(true)
			{
				auto i = var.find(':');
				if(i == std::string::npos)
				{
					ret.push_back(var);
					break;
				}
				else
				{
					auto comp = var.substr(0, i);
					ret.push_back(comp);
					var = var.substr(i + 1);
				}
			}

			return ret;
		}

		inline fs::path find_file_in_path(const fs::path& file, const std::vector<fs::path>& path)
		{
			for(auto& p : path)
			{
				auto tmp = (p / file);
				if(fs::exists(tmp))
					return tmp;
			}

			return { };
		}
	}

	inline Compiler find_c_compiler()
	{
		// TODO: support cross-compilation better
		// basically we need a way to define the target, provide a sysroot, and
		// find a specific "brand" of compiler, if you will.

		// for now, we just use the platform #defines to see which compiler to
		// search for. if we see _WIN32 but nothing suggesting cygwin or mingw,
		// then use MSVC's cl.exe. else, just use `cc` like a unix system.

	#if defined(_WIN32) && !defined(__CYGWIN__) && !defined(__MINGW32__) && !defined(__MINGW64__)

		#error "msvc is not supported yet"

	#else
		// basically, find 'cc' in the path.
		auto path_env = impl::get_path_variable();
		if(auto clang = impl::find_file_in_path("clang", path_env); !clang.empty())
			return { .path = clang, .kind = Compiler::KIND_GCC_CLANG, .lang = Compiler::LANG_C };

		else if(auto gcc = impl::find_file_in_path("gcc", path_env); !gcc.empty())
			return { .path = gcc, .kind = Compiler::KIND_GCC_CLANG, .lang = Compiler::LANG_C };

		else if(auto cc = impl::find_file_in_path("cc", path_env); !cc.empty())
			return { .path = cc, .kind = Compiler::KIND_GCC_CLANG, .lang = Compiler::LANG_C };

		else
			impl::error("could not find any C compiler in the $PATH");

	#endif // _WIN32
	}

	inline Compiler find_cpp_compiler()
	{
		// TODO: support cross-compilation better
		// basically we need a way to define the target, provide a sysroot, and
		// find a specific "brand" of compiler, if you will.

		// for now, we just use the platform #defines to see which compiler to
		// search for. if we see _WIN32 but nothing suggesting cygwin or mingw,
		// then use MSVC's cl.exe. else, just use `cc` like a unix system.

	#if defined(_WIN32) && !defined(__CYGWIN__) && !defined(__MINGW32__) && !defined(__MINGW64__)

		#error "msvc is not supported yet"

	#else
		// basically, find 'c++' in the path.
		auto path_env = impl::get_path_variable();
		if(auto clang = impl::find_file_in_path("clang++", path_env); !clang.empty())
			return { .path = clang, .kind = Compiler::KIND_GCC_CLANG, .lang = Compiler::LANG_CPP };

		else if(auto cc = impl::find_file_in_path("c++", path_env); !cc.empty())
			return { .path = cc, .kind = Compiler::KIND_GCC_CLANG, .lang = Compiler::LANG_CPP };

		else if(auto gcc = impl::find_file_in_path("g++", path_env); !gcc.empty())
			return { .path = gcc, .kind = Compiler::KIND_GCC_CLANG, .lang = Compiler::LANG_CPP };

		else
			impl::error("could not find any C++ compiler in the $PATH");

	#endif // _WIN32
	}

	inline void add_flags_for_compiler(const Compiler& com, std::vector<std::string>& flags, const CompilerFlags& opts)
	{
		flags.insert(flags.end(), opts.options.begin(), opts.options.end());

		if(com.kind == Compiler::KIND_GCC_CLANG)
		{
			for(auto& inc : opts.include_paths)
				flags.push_back(zpr::sprint("-I{}", inc.native()));
		}
		else
		{
			abort();
		}
	}
}




// actual compiling
namespace nabs
{
	inline bool compile_files(const CompilerFlags& opts, const fs::path& output, const std::vector<fs::path>& files)
	{
		bool is_cpp = false;
		for(auto& p : files)
		{
			auto ext = p.extension();
			if(ext == "cc" || ext == "cxx" || ext == "c++")
			{
				is_cpp = true;
				break;
			}
		}

		std::vector<std::string> args = opts.options;
		auto cmp = (is_cpp ? find_cpp_compiler() : find_c_compiler());

		if(cmp.kind == Compiler::KIND_GCC_CLANG)
		{
			for(auto& inc : opts.include_paths)
				args.push_back(zpr::sprint("-I{}", inc.native()));

			// TODO: need a way to specify default c/c++ standard
			if(cmp.lang == Compiler::LANG_CPP)      args.push_back("-std=c++17");
			else if(cmp.lang == Compiler::LANG_C)   args.push_back("-std=c11");

			args.push_back("-o");
			args.push_back(output.native());
		}
		else
		{
			impl::error("unsupported compiler");
		}

		// assume all compilers let you just throw the input files at the end with no regard.
		for(auto& f : files)
			args.push_back(f.native());

		// construct the command, and run it.
		return cmd(cmp.path.native(), std::move(args)).run();
	}

	template <typename... Args, typename = std::enable_if_t<((std::is_convertible_v<Args, fs::path>) && ...)>>
	inline bool compile_files(const CompilerFlags& opts, const fs::path& output, Args&&... inputs)
	{
		return compile_files(opts, output, std::vector<fs::path> { inputs... });
	}
}












