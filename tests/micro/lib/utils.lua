-- luacheck:globals import linter string
-- luacheck:std lua51c
-- luacheck:ignore 131
-- luacheck:ignore 42./.*_
-- luacheck:ignore 43./.*_
-- luacheck:module

local micro = import("micro")
local strings = import("strings")
local filepath = import("filepath")
local util = import("micro/util")
local path = import("path")
local os = import("os")
local errors = import("errors")

function is_whitespace(text)
    return text:gsub("%s+", ""):len() == 0
end


function switch(var, trueval, falseval)
    if var then return trueval else return falseval end
end


array = {
    map = function(arr, fn)
        local count = 0
        local out = {}
        for _, v in pairs(arr) do
            local val = fn(v)
            if val ~= nil then
                count = count + 1
                out[count] = v
            end
        end
        return out, count
    end,

    filter = function(arr, fn)
        local count = 0
        local out = {}
        for _, v in pairs(arr) do
            if fn(v) then
                count = count + 1
                out[count] = v
            end
        end
        return out, count
    end,

    count = function(arr, fn)
        local count = 0
        for _, v in pairs(arr) do
            if fn(v) then count = count + 1 end
        end
        return count
    end,

    item = function(arr, index) return arr[index] end,

    new = function (arr)
        -- Converts a go array into a lua table
        -- and returns it and it's length.
        if type(arr) == "table" then
            return arr, #arr
        end

        local out_arr = {}
        local i = 1
        while pcall(array.item, arr, i) do
            out_arr[i] = arr[i]
            i = i + 1
        end

        return out_arr, i - 1
    end,

    join = function (arr, joiner)
        joiner = joiner or " "
        local builder = strings.Builder()
        local len = #arr
        local ind = 0

        for _, v in pairs(arr) do
            ind = ind + 1
            builder:WriteString(v)
            if ind ~= len then builder:WriteString(joiner) end
        end

        return builder:String()
    end
}


info = {
    _err = nil,
    set = function(msg) micro.InfoBar():Message(msg) end,
    has_error = function() return info._err ~= nil end,
    set_error = function(msg) info._err = msg ; micro.InfoBar():Error(msg) end,
    get_error = function() return info._err end,
    clear_error = function() info._err = nil ;  micro.InfoBar():Reset() end
}


function string:split(splitter)
    local results = strings.Split(self, splitter)
    return array.new(results)
end


function string:lines()
    return self:split("\n")
end


function string:last_index(substr)
    return string.find(self, substr.."[^"..substr.."]*$")
end


function abspath(p)
    local val, err = filepath.Abs(p)
    if err then info.set_error(err) return nil end
    return util.String(val)
end


function paths_equal(p1, p2)
    return abspath(p1) == abspath(p2)
end


function filename(b)
    return util.String(path.Base(b.Path))
end


function folder(b)
    return util.String(path.Base(path.Dir(b.AbsPath)))
end

function args_to_string(args)
    args = array.new(args)
    return array.join(args, " ")
end


function table_join_keys(tbl, joiner)
    joiner = joiner or " "
    local builder = strings.Builder()
    local len = #tbl
    local ind = 0

    for v, _ in pairs(tbl) do
        ind = ind + 1
        builder:WriteString(v)
        if ind ~= len then builder:WriteString(joiner) end
    end

    return builder:String()
end

OR, XOR, AND = 1, 3, 4
function bitop(a, b, oper)
   local r, m, s = 0, 2^31
   repeat
      s,a,b = a+b+m, a%m, b%m
      r,m = r + m*oper%(s-a-b), m/2
   until m < 1
   return r
end

function is_executable(abspath)
    local fi, err = os.Stat(abspath)
    if err then return false end
    return bitop(fi:Mode(), 1, AND) ~= 0
end

function file_exists(abspath)
    local _, err = os.Stat(abspath)
    return not errors.Is(err, os.ErrNotExist)
end


cache = {}
function cached_fn(cache_name, fn)
    cache[cache_name] = {}

    return function (b)
        if cache[cache_name][b.AbsPath] == nil then
            cache[cache_name][b.AbsPath] = fn(b)
        end

        return cache[cache_name][b.AbsPath]
    end
end

function clear_cache()
    for k, _ in pairs(cache) do
        cache[k] = {}
    end
end
