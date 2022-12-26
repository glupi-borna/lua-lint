-- luacheck:globals import linter string
-- luacheck:std lua51c
-- luacheck:ignore 131
-- luacheck:ignore 42./.*_
-- luacheck:ignore 43./.*_
VERSION = "1.0.0"
local config = import("micro/config")
local buffer = import("micro/buffer")
local shell = import("micro/shell")
local micro = import("micro")
local strings = import("strings")
local path = import("path")
local filepath = import("path/filepath")
local ioutil = import("ioutil")
local os = import("os")
local fmt = import("fmt")

require("lib.utils")
local helper = require("lib.helper")

function remove_linter(name)
    if linter then linter.removeLinter(name) end
end

function make_linter(name, filetype, cmd, args, errorformat)
    if linter then
        linter.makeLinter(name, filetype, cmd, args, errorformat)
    end
end


function previous_empty_line(bp, cur)
    local line = cur.Y - 1

    if is_whitespace(bp.Buf:Line(line)) then
        while line > 0 and is_whitespace(bp.Buf:Line(line)) do
            line = line - 1
        end
    else
        while line > 0 and not is_whitespace(bp.Buf:Line(line)) do
            line = line - 1
        end
    end

    return math.max(line, 0)
end

function next_empty_line(bp, cur)
    local line = cur.Y + 1

    if is_whitespace(bp.Buf:Line(line)) then
        while line < bp.Buf:LinesNum() and is_whitespace(bp.Buf:Line(line)) do
            line = line + 1
        end
    else
        while line < bp.Buf:LinesNum() and not is_whitespace(bp.Buf:Line(line)) do
            line = line + 1
        end
    end

    return math.min(line, bp.Buf:LinesNum() + 1)
end

function maybe_start_selection(cur)
    if not cur:HasSelection() then
        cur.OrigSelection[1] = buffer.Loc(cur.X, cur.Y)
    end
end

function select_paragraph_prev(bp)
    for i = 0, bp.Buf:NumCursors() - 1 do
        local cur = bp.Buf:GetCursor(i)
        maybe_start_selection(cur)

        local line = previous_empty_line(bp, cur)
        cur:SelectTo(buffer.Loc(0, line))

        cur.Y = line
        cur.X = 0
        cur:Relocate()
    end

    bp:Relocate()
end

function select_paragraph_next(bp)
    for i = 0, bp.Buf:NumCursors() - 1 do
        local cur = bp.Buf:GetCursor(i)
        maybe_start_selection(cur)

        local line = next_empty_line(bp, cur)
        cur:SelectTo(buffer.Loc(0, line))

        cur.Y = line
        cur.X = 0
        cur:Relocate()
    end

    bp:Relocate()
end

function move_paragraph_prev(bp)
    for i = 0, bp.Buf:NumCursors() - 1 do
        local cur = bp.Buf:GetCursor(i)
        local line = previous_empty_line(bp, cur)

        cur.Y = line
        cur.X = 0
        cur:Relocate()
    end

    bp:Relocate()
end

function move_paragraph_next(bp)
    for i = 0, bp.Buf:NumCursors() - 1 do
        local cur = bp.Buf:GetCursor(i)
        local line = next_empty_line(bp, cur)

        cur.Y = line
        cur.X = 0
        cur:Relocate()
    end

    bp:Relocate()
end

function move_message_prev(bp, _args)
    local args, argc = array(_args)
    local msgkind = nil
    if argc > 0 then msgkind = args[1] end

    if not msgkind then
        msgkind = buffer.MTError
    else
        local name = msgkind
        msgkind = buffer[name]
        if not msgkind then
            micro.TermMessage("Unknown message kind '" .. name .. "'")
            return
        end
    end

    local msgs, len = array(bp.Buf.Messages)
    for i=0, bp.Buf:NumCursors() - 1 do
        local cur = bp.Buf:GetCursor(i)
        local next = -1

        for mi=1, len do
            local msg = msgs[mi]
            local invalid = msg.Kind ~= msgkind or
                            msg.Start.Y == -1 or
                            msg.Start.Y >= cur.Y or
                            msg.Start.Y < next
            if not invalid then
                next = msg.Start.Y
            end
        end

        if next > -1 then
            cur.Y = next
            cur.X = 0
            cur:Relocate()
        end
    end

    bp:Relocate()
end

function move_message_next(bp, _args)
    local args, argc = array(_args)
    local msgkind = nil
    if argc > 0 then msgkind = args[1] end

    if not msgkind then
        msgkind = buffer.MTError
    else
        local name = msgkind
        msgkind = buffer[name]
        if not msgkind then
            micro.TermMessage("Unknown message kind '" .. name .. "'")
            return
        end
    end

    local msgs, len = array(bp.Buf.Messages)
    for i=0, bp.Buf:NumCursors() - 1 do
        local cur = bp.Buf:GetCursor(i)
        local next = bp.Buf:LinesNum() * 2

        for mi=1, len do
            local msg = msgs[mi]
            local invalid = msg.Kind ~= msgkind or
                            msg.Start.Y == -1 or
                            msg.Start.Y <= cur.Y or
                            msg.Start.Y > next
            if not invalid then
                next = msg.Start.Y
            end
        end

        if next < bp.Buf:LinesNum() * 2 then
            cur.Y = next
            cur.X = 0
            cur:Relocate()
        end
    end

    bp:Relocate()
end

function get_indent(buf, line)
    local text = buf:Line(line)
    if is_whitespace(text) then return "" end

    local first_char = text:sub(1,1)

    if first_char == "\t" then return first_char end

    if first_char == " " then
        local len = 1
        local indent = " "

        while true do
            local next_char = text:sub(len+1, len+1)
            if next_char == " " then
                len = len + 1
                indent = indent .. " "
            else
                return indent
            end
        end
    end

    return ""
end


function detect_indent(buf)
    local line = 0
    local lines_num = buf:LinesNum()
    local line_indent
    local last_line_indent = 0
    local buckets = {}

    while line < lines_num do
        line = line + 1
        local indent_str = get_indent(buf, line)
        line_indent = #indent_str
        if line_indent ~= last_line_indent then
            if indent_str == "\t" then
                buckets["\t"] = (buckets["\t"] or 0) + 1
            elseif line_indent > 0 then
                local diff = math.abs(line_indent - last_line_indent)
                buckets[diff] = (buckets[diff] or 0) + 1
            end
        end
        last_line_indent = line_indent
    end

    local largest_key = ""
    local largest_value = 0

    for k, v in pairs(buckets) do
        if v > largest_value then
            largest_key = k
            largest_value = v
        end
    end

    if largest_key == "\t" then
        buf.Settings["tabstospaces"] = false
    elseif largest_value > 0 then
        buf.Settings["tabstospaces"] = true
        buf.Settings["tabsize"] = largest_key
    end
end


function detect_indent_bp(bp)
    return detect_indent(bp.Buf)
end


function git_root(b)
    if b.Type.Kind ~= buffer.BTInfo then
        local root, err = shell.ExecCommand("git", "rev-parse", "--show-toplevel")
        if err == nil then
            return strings.TrimSpace(path.Base(root))
        end
    end
    return ""
end


function git_remote(b)
    if b.Type.Kind ~= buffer.BTInfo then
        local remote, err = shell.ExecCommand("git", "config", "--local", "remote.origin.url")
        if err == nil then
            remote = strings.TrimSpace(remote:sub(remote:last_index("/") + 1))

            if remote:sub(-4, -1) == ".git" then
                remote = remote:sub(0, -5)
            end

            return remote
        end
    end
    return ""
end

function git_project(b)
    local rmt = git_remote(b)
    if rmt ~= "" then return rmt end

    local rt = git_root(b)
    if rt ~= "" then return rt end

    local fd = folder(b)
    if fd ~= "" then return fd end

    return ""
end

function git_dirty(b)
    if b.Type.Kind ~= buffer.BTInfo then
        local dirty, err = shell.ExecCommand("git", "diff-index", "HEAD", "--")
        if err == nil and dirty ~= "" then
            return strings.TrimSpace("*")
        end
    end
    return ""
end

function git_branch(b)
    if b.Type.Kind ~= buffer.BTInfo then
        local branch, err = shell.ExecCommand("git", "rev-parse", "--abbrev-ref", "HEAD")
        if err == nil then
            return "@" .. strings.TrimSpace(branch)
        end
    end
    return ""
end

function git_blame(bp)
    local b = bp.Buf
    if b.Type.Kind ~= buffer.BTInfo and b:NumCursors() == 1 then
        local cursor = b:GetCursor(0)
        local blame, err = shell.ExecCommand("git", "blame", "-L", cursor.Loc.Y+1 .. "," .. cursor.Loc.Y+1, b.AbsPath)
        if err ~= nil then return "" end

        local parts, _ = blame:split(" ")
        local hash = strings.TrimSpace(parts[1])
        if hash == "000000000" or hash == "00000000" then return "Uncommitted" end
        if hash:sub(1, 1) == "^" then hash = hash:sub(2) end

        local author, err2 = shell.ExecCommand("git", "log", "--format=%an", hash.."^!")
        if err2 ~= nil then return "" .. err2 end
        return strings.TrimSpace(author)
    end
    return ""
end

function git_info(b)
    local branch = git_branch(b)
    if branch == "" then return folder(b) end

    local dirty = git_dirty(b)
    local project = git_project(b)

    return project .. branch .. dirty
end

git_root_cached = cached_fn("git_root", git_root)
git_remote_cached = cached_fn("git_remote", git_remote)
git_project_cached = cached_fn("git_project", git_project)
git_dirty_cached = cached_fn("git_dirty", git_dirty)
git_branch_cached = cached_fn("git_branch", git_branch)
git_info_cached = cached_fn("git_info", git_info)

function lsp_status(b)
    if b:HasLSP() then
        local s = b.Server
        return s:GetLanguage().Command
    else
        return "LSP off"
    end
end

function lsp_restart(bp)
    local b = bp.Buf
    if b:HasLSP() then
        bp:SetLocalCmd({"lsp", "off"})
    end
    bp:SetLocalCmd({"lsp", "on"})
end

function preSave(_)
    clear_cache()
end


function buf_set_text(buf, text)
    local eh = buf.EventHandler
    eh:Remove(
        buffer.Loc(0, 0),
        buf:End())
    eh:Insert(buffer.Loc(0, 0), text)
end


function message_count(b)
    local msgs, _ = array.new(b.Messages)
    return array.count(msgs, function(msg) return msg.Kind ~= buffer.MtMark end)
end

function get_diagnostics(b)
    if not b:HasLSP() then return {}, 0 end
    local diags, len = array.new(b.Server:GetDiagnostics(b.AbsPath))
    return diags, len
end

function message_diagnostic_count(b)
    local count = message_count(b)
    local _, dlen = get_diagnostics(b)
    return count + dlen
end

function show_messages(bp)
    if helper.close(bp, "Messages") then return end

    local arr, len = array.new(bp.Buf.Messages)
    local diags, dlen = get_diagnostics(bp.Buf)

    if len + dlen == 0 then return end

    local text = ""
    local i = 1
    local count = 0

    while i <= len do
        local msg = arr[i]
        if msg.Kind ~= buffer.MTMark then
            if msg.Start.Y ~= -1 then
                text = text .. tostring(msg.Start.Y + 1) .. ": " .. msg.Msg .. "\n"
            else
                text = text .. msg.Msg .. "\n"
            end
            count = count + 1
        end
        i = i+1
    end

    i = 1
    while i <= dlen do
        local diag = diags[i]
        text = text .. tostring(diag.Range.Start.Line + 1) .. ": " .. diag.Message .. "\n"
        count = count + 1
        i = i+1
    end

    if count == 0 then return end

    local h = helper.open("Messages", "h", bp:GetView().Height-10, nil, bp.Buf)
    buf_set_text(h, text)
end

function maybe_open_file(bp, file)
    if file == nil then return end
    local splitstr = file:split(":")
    local file_path = strings.TrimSpace(splitstr[1])
    local line = nil
    if splitstr[2] then
        line = strings.TrimSpace(splitstr[2])
    end

    local old_buffer = bp.Buf
    if not paths_equal(old_buffer.AbsPath, file_path) then
        bp:OpenCmd{file_path}
        old_buffer:Fini()
    end

    if line then
        local cur = bp.Buf:GetActiveCursor()
        cur.Y = tonumber(line) - 1
    end
end

function glob(_glob) return '--glob "' .. _glob .. '" ' end

local rg_globs =
    glob("**/*") ..
    glob("**/micro*") ..
    glob("!.git") ..
    glob("!**/node_modules/*") ..
    glob("!**/.dub/*") ..
    glob("!**/dist/*") ..
    glob("!**/.mypy_cache/*") ..
    glob("!**/__pycache__/*") ..
    glob("!**/venv/*")

local rg_params = '--no-heading --no-hidden --follow --sort-files -S ' .. rg_globs

local lib_map = {
    ["unknown"]="$SHELLUTILS/libs",
    ["c"]="$SHELLUTILS/libs/c",
    ["c++"]="$SHELLUTILS/libs/c",
    ["lua"]="$SHELLUTILS/libs/lua",
    ["python"]="$SHELLUTILS/libs/py"
}

function get_rg_params(ft, query)
    local paths = " . "
    if lib_map[ft] ~= nil then
        paths = paths .. lib_map[ft] .. ' '
    end

    if query ~= nil then
        return rg_params .. ' ' .. query .. ' ' .. paths .. " 2>/dev/null "
    else
        return rg_params .. paths .. " 2>/dev/null "
    end
end

function ripgrep(filetype, query, files, command_only)
    files = switch(files, "--files", "--line-number")
    command_only = switch(command_only, true, false)

    local params = files .. " " .. get_rg_params(filetype, query)
    local command = 'rg ' .. params

    if command_only then return command end

    local val, err = shell.RunInteractiveShell("sh -c '" .. command .. "'", false, true)
    if err ~= nil then info.set_error(err); return nil end
    return val:split("\n")
end

function get_fzf_params(with_line_number, autoreturn)
    with_line_number = switch(with_line_number, "--highlight-line {2}", "")
    autoreturn = switch(autoreturn, "-1 ", "")
    return autoreturn .. '--delimiter : ' ..
        '--preview "bat --color=always {1} ' .. with_line_number .. '" ' ..
        '--border sharp ' ..
        '--preview-window "' ..
            "up,50%,wrap,+{2}+3/2,~3" ..
        '"'
end

function fzf_arr(arr, line_highlight, autoreturn, query)
    query = switch(query, '-q="' .. (query or "") .. '" ', "")

    local set = {}
    for _, v in pairs(arr) do
        set[v] = true
    end

    local items = table_join_keys(set, "\n")

    local command = "bash -c '" ..
        'fzf ' .. query ..
        get_fzf_params(line_highlight, autoreturn) ..
        "<<EOF\n" .. strings.TrimSpace(items) .. "\nEOF" ..
    "'"

    local val, err = shell.RunInteractiveShell(command, false, true)
    if err ~= nil then info.set_error(fmt.Sprint("FZF: ", err)) return nil end
    if val == "" then return nil end
    return val
end

function fzf_cmd(cmd, line_highlight, autoreturn, query, disabled)
    query = switch(query, '-q "' .. (query or "") .. '" ', "")
    disabled = switch(disabled, '--disabled ', '')

    local command = "fzf " .. disabled .. query ..
        "--bind 'change:reload(" .. cmd .. " || true)' " ..
        get_fzf_params(line_highlight, autoreturn)

    local val, err = shell.RunInteractiveShell(command, false, true)
    if err ~= nil then info.set_error(fmt.Sprint("FZF: ", err)) return nil end
    if val == "" then return nil end
    return val
end

function fzf(bp, args)
    info.clear_error()
    local query = args_to_string(args)
    if query == "" then query = nil end

    local result = fzf_cmd(ripgrep(bp, nil, true, true), false, false, query, false)
    if info.has_error() then return end

    maybe_open_file(bp, result)
end

function gfzf(bp, args)
    info.clear_error()
    local query = args_to_string(args)
    if query == "" then query = nil end

    local result = fzf_cmd(ripgrep(bp, "{q}", false, true), true, false, query, true)


    if info.has_error() then return end
    maybe_open_file(bp, result)
end

function insert_text(bp, args)
    local text = args_to_string(args)
    if #text == 0 then
        return
     end

    for i = 0, bp.Buf:NumCursors() - 1 do
        local cur = bp.Buf:GetCursor(i)
        bp.Buf.EventHandler:Insert(-cur.Loc, text)
    end
end

function run_command(_, args)
    local cmd = args_to_string(args)
    shell.RunInteractiveShell(cmd, false, true)
end

function recurse_find_file(filename, initial_dir)
    local dir = initial_dir or os.Getwd()

    local found = false
    while not found do
        local dir_scan, scan_error = ioutil.ReadDir(dir)

        if scan_error then break end
        if dir_scan == nil then break end

        for i = 1, #dir_scan do
            local entry = dir_scan[i]
            if not entry:IsDir() then
                local name = entry:Name()
                if name == filename then
                    return filepath.Join(dir, name), dir
                end
            end
        end

        local parent_dir = filepath.Dir(dir)
        if parent_dir == dir then break else dir = parent_dir end
    end

    return nil, nil
end

--[[
local function find_file(filename, dir)
    dir = dir or os.Getwd()

    local dir_scan, scan_error = ioutil.ReadDir(dir)
    if scan_error then return nil end
    if dir_scan == nil then return nil end

    for i=1, #dir_scan do
        local entry = dir_scan[i]
        if not entry:IsDir() then
            local name = entry:Name()
            if name == filename then
                return filepath.Join(dir, name)
            end
        end
    end

    return nil
end
]]

local frotate
frotate = {
    init = function()
        config.RegisterCommonOption("frotate", "frotate_source", nil)
        config.RegisterCommonOption("frotate", "frotate_targets", nil)
    end,

    conf_match_index = function(abspath, conf)
        if conf == nil then return false end

        for i, v in conf() do
            if strings.HasSuffix(abspath, v) then return i end
        end

        return -1
    end,

    run = function(bp)
        local abspath = bp.Buf.AbsPath
        local confs = bp.Buf.Settings["frotate"]

        if confs == nil then return end

        local conf = nil
        local conf_index = -1
        for _, v in confs() do
            local ind = frotate.conf_match_index(abspath, v)
            if ind ~= -1 then
                conf = v
                conf_index = ind
                break
            end
        end

        if conf == nil or conf_index == -1 then return end

        local noext = strings.TrimSuffix(abspath, conf[conf_index])

        local len = #conf
        local target = nil
        for i=0, len-2 do
            local ind = (i + conf_index) % len + 1
            local candidate = noext .. conf[ind]
            if file_exists(candidate) then
                target = candidate
                break
            end
        end

        maybe_open_file(bp, target)
    end
}

function set_comment_types(buf, ft_ct)
    local ft = buf.Settings["filetype"]

    if ft_ct[ft] then
        buf.Settings["commenttype"] = ft_ct[ft]
    end
end

function get_marks(buf)
    local arr, len = array.new(buf.Messages)
    local marks = {}
    local count = 0

    for i=1, len do
        local msg = arr[i]
        if msg.Kind == buffer.MTMark then
            count = count + 1
            marks[count] = msg.Start.y + 1
        end
    end

    return marks, count
end

function marks_string(buf, fmt)
    if fmt == nil then fmt = "%s:%d" end
    local marks, len = get_marks(buf)
    local text = ""
    for i=1, len do
        text = text .. string.format(fmt, buf.AbsPath, marks[i]) .. " "
    end
    return text
end

function run_term(bp, cmd)
    if shell.TermEmuSupported then
        shell.RunTermEmulator(bp, cmd, true, false, nil, nil)
        return "", false
    else
        local msg, err = shell.RunCommand(cmd)
        return msg, err
    end
end

function microbuild(bp, args)
    local file = recurse_find_file(".microbuild")

    if not file then
        if is_executable(bp.Buf.AbsPath) then
            run_term(bp, bp.Buf.AbsPath)
            return
        else
            micro.TermMessage("Missing .microbuild file in hierarchy, don't know how to build.")
            return
        end
    end

    local cmd = file .. " " .. args_to_string(args) .. " :::: " .. marks_string(bp.Buf)
    local msg, err = run_term(bp, cmd)

    if err then
        micro.TermMessage("ERROR: " ..  tostring(err) .. "\n" .. tostring(msg))
    end
end

function microlint()
    if not linter then
        return
    end

    local file, dir = recurse_find_file(".microlint")

    if not file then
        return
    end

    local text, err = shell.ExecCommand("cat", file);

    if err then
        micro.TermMessage(err)
        return
    end

    local lines = text:lines()
    for _, line in pairs(lines) do
        local trimline = strings.TrimSpace(line)

        if trimline ~= "" and trimline:sub(1, 1) ~= "#" then
            local lintconf = line:split(" :||: ")
            local name, filetype, cmd, argstr, errorformat = unpack(lintconf)
            name = strings.TrimSpace(name)
            filetype = strings.TrimSpace(filetype)
            cmd = strings.TrimSpace(cmd)
            argstr = strings.TrimSpace(argstr)
            errorformat = strings.TrimSpace(errorformat)

            if name == nil or filetype == nil or cmd == nil or argstr == nil or errorformat == nil then
                micro.TermMessage(
                    "Incorrectly formatted .microlint line.\n"
                    .. "\tname: " .. tostring(name) .. "\n"
                    .. "\tfiletype: " .. tostring(filetype) .. "\n"
                    .. "\tcmd: " .. tostring(cmd) .. "\n"
                    .. "\targs: " .. tostring(argstr) .. "\n"
                    .. "\terrorformat: " .. tostring(errorformat) .. "\n"
                )
                return
            end

            local args = argstr:split(" ++ ")
            for index, arg in pairs(args) do
                args[index] = strings.TrimSpace(arg):gsub("%%D", dir)
            end

            remove_linter(name)
            make_linter(name, filetype, cmd, args, errorformat)
        end
    end
end

function marks_to_string(buf)
    local fmt = " :%d"
    local marks, len = get_marks(buf)
    local text = buf.AbsPath
    for i=1, len do
        local mark = marks[i]
        text = text .. string.format(fmt, mark.line)
    end
    return text
end

function marks_register(buf, bits, len)
    for i=2, len do
        local linenum = tonumber(bits[i])
        local mark = buffer.NewMessageAtLine("breakpoint", "", linenum, buffer.MTMark)
        buf:AddMessage(mark)
    end
end

function marks_parse(buf, marksfile)
    local lines, len = marksfile:split("\n")
    for i=1, len do
        local line=lines[i]
        local bits = line:split(" :")
        local file = bits[1]

        if file == buf.AbsPath then
            marks_register(buf, bits, len)
        end
    end
end

function marks_load(buf)
    local file = config.ConfigDir .. "/.micromarks"
    local text, err = shell.ExecCommand("cat", file)

    if err then return end
    marks_parse(buf, text)
end

--[[
function marks_save(buf)
    local text = marks_to_string(buf)
    local msg, err = shell.RunCommand(cmd)
    -- @TODO: Finish this sometime (write .micromarks file
    --        and make this run in onBufferOpen)
end
]]

function preQuit(bp)
    helper.close_all(bp)
end

function onBufferOpen(buf)
    local comment_types = {
        typescript = "// %s",
        typescriptreact = "// %s",
        processing = "// %s",
    }
    set_comment_types(buf, comment_types)
    detect_indent(buf)
    microlint()
end

function superline(arr, buff, pre, post)
    pre = pre or ""
    post = post or ""
    local out = ""

    for _, v in pairs(arr) do
        local val
        local t = type(v)
        if t == "table" then
            val = superline(v.arr, buff, v.pre, v.post)
        elseif t == "function" then
            val = v(buff)
        else
            val = v
        end
        local ok = val ~= "" and val ~= nil
        if ok then
            out = out .. pre .. val .. post
        end
    end

    return out
end

function superline_left(b)
    local modified = b:Modified() and "• " or ""
    return superline({
        {arr={git_project_cached, git_branch_cached}},
        modified .. b:GetName(),
    }, b, " ", " │")
end

function superline_right(b)
    local msgs = message_diagnostic_count(b)
    if msgs > 0 then msgs = tostring(msgs) .. " msgs" else msgs = "" end
    return superline({
        lsp_status,
        msgs,
        b.Settings["filetype"]
    }, b, "│ ", " ")
end

function string:starts(start)
   return self:sub(1, start:len()) == start
end


function loc_to_string(loc, nilname)
    if loc == nil then return "" end
    local fn = tostring(loc.URI)
    if fn:starts("file://") then
        fn = fn:sub(8)
    end
    if fn == "" then fn = nilname end
    return fn .. ":" .. (loc.Range.Start.Line+1) .. ":" .. (loc.Range.Start.Character+1)
end

function locs_to_strings(locs, nilname)
    local out = {}
    local ind = 1
    for _, loc in pairs(locs) do

        if loc ~= "" then
            out[ind] = loc_to_string(loc, nilname)
            ind = ind + 1
        end
    end
    return out
end

function init()
    frotate.init()

    remove_linter("luacheck")
    remove_linter("eslint")
    remove_linter("mypy")
    remove_linter("shellcheck")
    remove_linter("dmd")
    -- remove_linter("flake8")
    make_linter("luacheck", "lua", "luacheck", {"--no-color", "--codes", "-t", "%f"}, "%f:%l:%c: %m")
    make_linter("godot", "gdscript", "godotcheck", {"%f"}, "%f:%l: %m")
    make_linter(
        "mypy", "python",
        "python3", {
            "-m", "mypy",
            "--install-types",
            "--non-interactive",
            "%f"
        },
        "%f:%l: %m")

    local shellcheck_excludes = "SC2046,SC2155,SC2219,SC2164"
    make_linter(
        "shellcheck", "shell",
        "shellcheck", {
            "-s", "bash",
            "-e", shellcheck_excludes,
            "-f", "gcc",
            "-x", "%f"
        },
        "%f:%l:%c: %m")
    -- make_linter("denolint", "typescript", "scr-deno-lint", {"%f"}, '"%f:%l: %m"')

    micro.SetStatusInfoFn("user.wordcount")
    micro.SetStatusInfoFn("user.charcount")
    micro.SetStatusInfoFn("user.linecount")
    micro.SetStatusInfoFn("user.folder")
    micro.SetStatusInfoFn("user.filename")
    micro.SetStatusInfoFn("user.git_root")
    micro.SetStatusInfoFn("user.git_root_cached")
    micro.SetStatusInfoFn("user.git_remote")
    micro.SetStatusInfoFn("user.git_remote_cached")
    micro.SetStatusInfoFn("user.git_project")
    micro.SetStatusInfoFn("user.git_project_cached")
    micro.SetStatusInfoFn("user.git_dirty")
    micro.SetStatusInfoFn("user.git_dirty_cached")
    micro.SetStatusInfoFn("user.git_branch")
    micro.SetStatusInfoFn("user.git_branch_cached")
    micro.SetStatusInfoFn("user.git_info")
    micro.SetStatusInfoFn("user.git_info_cached")
    micro.SetStatusInfoFn("user.git_blame")
    micro.SetStatusInfoFn("user.message_count")
    micro.SetStatusInfoFn("user.lsp_status")
    micro.SetStatusInfoFn("user.superline_left")
    micro.SetStatusInfoFn("user.superline_right")
    config.MakeCommand("fzf", fzf, config.NoComplete)
    config.MakeCommand("gfzf", gfzf, config.NoComplete)
    config.MakeCommand("insert-text", insert_text, config.NoComplete)
    config.MakeCommand("lsp-restart", lsp_restart, config.NoComplete)
    config.MakeCommand("message-next", move_message_next, config.NoComplete)
    config.MakeCommand("message-prev", move_message_prev, config.NoComplete)
    config.MakeCommand("messages-show", show_messages, config.NoComplete)
    config.MakeCommand("microbuild", microbuild, config.NoComplete)
    config.MakeCommand("microlint", microlint, config.NoComplete)
    config.MakeCommand("indentation-detect", detect_indent_bp, config.NoComplete)

    config.MakeCommand("indentation-change", function(bp, _args)
        local buf = bp.Buf
        local args, _ = array(_args)
        local new_indent = tonumber(args[1])
        local old_indent = buf.Settings["tabsize"]
        local old_tts = buf.Settings["tabstospaces"]
        if new_indent == old_indent then info.set("No change") return end

        buf.Settings["tabstospaces"] = false
        buf:Retab()
        buf.Settings["tabsize"] = new_indent
        buf.Settings["tabstospaces"] = old_tts
        buf:Retab()
    end, config.NoComplete)

    config.MakeCommand("blame", function (bp)
        info.set(git_blame(bp))
    end, config.NoComplete)

    local function arrayify(data)
        if not data then data = {} else data = array.new(data) end
        return data
    end

    config.MakeCommand("helper-close", function(bp) helper.close_all(bp) end, config.NoComplete)
    config.MakeCommand("helper-debug", helper.debug, config.NoComplete)

    config.MakeCommand("id", function(bp) info.set(tostring(bp.Buf.ID)) end, config.NoComplete)

    config.MakeCommand("catalog", function (bp)
        if not bp.Buf:HasLSP() then
            local word = bp.Buf:WordAtAsStr(-bp.Buf:GetActiveCursor().Loc)
            if word ~= nil and word ~= "" then
                gfzf(bp, {word})
                return
            end
        end

        local def_locs = arrayify(bp.Buf:LSPDefinition())
        local dec_locs = arrayify(bp.Buf:LSPDeclaration())
        local td_locs = arrayify(bp.Buf:LSPTypeDefinition())
        local ref_locs = arrayify(bp.Buf:LSPReferences())

        local all_locs = { unpack(def_locs), unpack(dec_locs), unpack(td_locs), unpack(ref_locs) }
        local locs = locs_to_strings(all_locs, bp.Buf.AbsPath)

        local val, err = fzf_arr(locs, true)
        if err ~= nil then info.set(tostring(err)) return end
        if val == nil then return end

        val = strings.TrimSpace(val)
        if val == "" then val = nil end
        if not val then info.set("No results") return end

        val = strings.TrimSpace(val):sub(1, -2)
        local action = strings.TrimSpace(fzf_arr({"Open", "Preview"}))

        if action == "Open" then
            maybe_open_file(bp, val)
        elseif action == "Preview" then
            helper.close(bp, "Catalog")
            helper.helperize(
                helper.open(
                    "Catalog", "h",
                    bp:GetView().Height-10,
                    buffer.NewBufferFromFile(val),
                    bp.Buf
                )
            )
        end
    end, config.NoComplete)

    config.MakeCommand("frotate", frotate.run, config.NoComplete)
end
