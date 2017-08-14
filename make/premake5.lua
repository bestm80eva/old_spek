-- Spek build script

rootdir = path.join(path.getdirectory(_SCRIPT), "..")

filter { "platforms:Win64" }
	system "Windows"
	architecture "x64"


-- Solution
solution "spek"
	language "C"
	configurations { "Debug", "Release" }
	platforms { "Win64" }
	location "../_build"
    debugdir "../data"
    characterset "MBCS"

	defines {
		"_CRT_SECURE_NO_WARNINGS",
	}

    linkoptions "/opt:ref"
    editandcontinue "off"

    rtti "off"
    exceptionhandling "off"

	configuration "Debug"
		defines { "_DEBUG" }
		flags { "FatalWarnings" }
		symbols "on"

	configuration "Release"
		defines { "NDEBUG" }
		optimize "full"

	-- Projects
	project "spek"
		targetdir "../_bin/%{cfg.platform}/%{cfg.buildcfg}/%{prj.name}"
		objdir "../_obj/%{cfg.platform}/%{cfg.buildcfg}/%{prj.name}"
		kind "WindowedApp"
		files {
			"../src/**.c",
		}
		includedirs {
			"../include",
		}

        -- postbuildcommands {
        --     "copy \"" .. path.translate(path.join(rootdir, "data")) .. '" "' ..
        --         path.translate(path.join(rootdir, "_Bin", "%{cfg.platform}", "%{cfg.buildcfg}", "%{prj.name}")) .. '"'
        -- }

		configuration "Win*"
			defines {
				"WIN32",
			}
			flags {
				"StaticRuntime",
				"NoMinimalRebuild",
				"NoIncrementalLink",
                "WinMain",
			}
