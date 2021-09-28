
%% Create a Flow Chart for the function
%
% Inputs:
%   - the Stateflow file handle
%   - the AST corresponding to the function
% Outputs: the chart corresponding to the function
function x_pos = createFunctionFlowChart(handle, func, x_pos)
	func_name  = func.decl.name;

	% Create the input string

	if isempty(func.decl.type.args)
		arg_str = '';
	else
		args = func.decl.type.args.params;
		arg_str = '';
		for i = 1:1:length(args)
			comma='';
			if i < length(args)
				comma = ', ';
			end
			arg_str =  [arg_str  args(i).name  comma];

		end
	end

	%Check to see if an output argument is nessary
	output_type = func.decl.type.type.type.names;
	outputs= '';
	if ~strcmp(output_type,'void')
		outputs = 'output = ';
	end

	%Creating the stateflow chart and setting t`he inputs and outputs
	func_def = [outputs func_name '(' arg_str ')'];
	delta = 200;

	flw_chrt = Stateflow.Function(handle);
	flw_chrt.LabelString = func_def;

	%default transition
	dt = Stateflow.Transition(flw_chrt);
	junc = Stateflow.Junction(flw_chrt);
	junc.Position.Center =[x_pos + delta/2.0 + 100,100];
	dt.SourceEndPoint = [x_pos + delta/2.0 + 100, 25];
	dt.Destination = junc;

	[last_trans, last_junc] = createFunctionBody(flw_chrt, dt, junc, func.body);
	width = delta*(getNestedDepth(func.body)+2);
	depth = last_junc.Position.Center(2) +delta/2.0;
	flw_chrt.Position =[x_pos + delta/2.0,1, width, depth];
	x_pos = x_pos + width + delta;
end

% createFunctionBody
%
%	Recursive function that builds the body of the function.
%
%	Inputs:
%   		flw_chrt:	The flow chart function that will be added two
%
%			prev_trans: The previously created transition
%
%   		curr_junc:	The junction that will be built off of
%
%    		node: 		The current node of the AST
%
% 	Outputs:
%   		trans: 		The last transition of the state chart (returns [] for if condition)
%
%   		junc: 		the last junction of the state chart
function [trans, junc] = createFunctionBody(flw_chrt, prev_trans, curr_junc, node)

	if isempty(node)
		junc = curr_junc;
		trans = prev_trans;
		return;
	end

	switch node.x_nodetype
		case 'Compound'
			[trans,junc] = createSetOfStatements(flw_chrt, prev_trans, curr_junc, node);
		case 'Decl'
			if strcmp(node.type.x_nodetype, 'TypeDecl') && strcmp(node.type.type.x_nodetype, 'IdentifierType')

			else
				statement = [getDeclaration(node)];
				[trans, junc] = createStatementTransition(...
					flw_chrt, prev_trans, curr_junc, statement);
			end
		case 'Assignment'
			statement = [getAssignment(node)];
			[trans, junc] = createStatementTransition(...
				flw_chrt,prev_trans, curr_junc, statement);
		case 'FuncCall'
			statement =  [getFunctionDef(node) ';'];
			[trans, junc] = createStatementTransition(flw_chrt, ...
				prev_trans, curr_junc, statement);
		case 'If'
			[trans,junc] = createIfStatement(flw_chrt, prev_trans, curr_junc, node);
		case 'Switch'
			[trans,junc] = createSwitchStatement(flw_chrt, prev_trans, curr_junc, node);
		case 'Return'
			statement = ['output = ' getAssignmentExp(node.expr) ';'];
			[trans, junc] = createStatementTransition(flw_chrt, prev_trans, curr_junc, statement);
		case 'UnaryOp'
			if strcmp(node.op, 'p--') || strcmp(node.op, 'p++')
				statement = [createCondition(node.expr) node.op ';'];
			elseif strcmp(node.op, '--') || strcmp(node.op, '++')
				statement = [node.op createCondition(node.expr) ';'];
			elseif strcmp(node.op, '!')
				statement = ['~(' createCondition(node.expr) ')'];
			else
				error(['Unsupported Unary Operator: ' node.x_nodetype]);
			end
			[trans, junc] = createStatementTransition(flw_chrt, prev_trans, curr_junc, statement);

		otherwise
			error(['Unsupported C Syntax: ' node.x_nodetype]);
	end

end


%% Create Stateflow for a compound set of statements
%Based on the AST, creates the set of statements
% Inputs:
%   - flw_chrt: The flow chart function that will be added two
%   - curr_junc: The junction that will be built off of
%   - node: The current node of the AST
%
%Outputs:
%   - The last transition of the state chart (returns [] for if condition)
%   - the last junction of the state chart
function [trans,junc] = createSetOfStatements(flw_chrt, prev_trans, curr_junc, node)
	blcks = node.block_items;
	next_junc = curr_junc;
	actions = [];

	%check to see if the length is one
	if length(blcks) ==1
		[prev_trans, next_junc] = createFunctionBody( ...
			flw_chrt, prev_trans, next_junc, blcks);
	else

		for i = 1:1:length(blcks)
			%check to see if its Decl, if so then store it and see if
			%there are multiple decl statements that can be
			%concatinated together before creating a junction for it

			if iscell(blcks)
				blck = blcks{i};
			else
				blck = blcks(i);
			end

			if strcmp(blck.x_nodetype,'Decl')
				if (strcmp(blck.type.x_nodetype, 'TypeDecl') && strcmp(blck.type.type.x_nodetype, 'IdentifierType'))
					actions = [actions getDeclaration(blck) newline];
				end
			elseif strcmp(blck.x_nodetype, 'Assignment')
				actions = [actions getAssignment(blck) newline];
			elseif strcmp(blck.x_nodetype, 'FuncCall')
				actions = [actions getFunctionDef(blck) ';' newline ];
			else
				%if the last statement was not the inital value for
				%decl then add a transition the declaritive actions
				if ~strcmp(actions, '')
					[prev_trans, next_junc] = createStatementTransition(...
						flw_chrt,prev_trans, next_junc, actions);
					actions = '';
				end

				%recursively call for the next body
				[prev_trans, next_junc] = createFunctionBody( ...
					flw_chrt, prev_trans, next_junc, blck);
			end
		end

	end

	if ~isempty(actions) && ~strcmp(actions, '') && ~strcmp(erase(actions,newline), '{}')
		[prev_trans, next_junc] = createStatementTransition(...
			flw_chrt,prev_trans, next_junc, actions);
	end

	trans =prev_trans;
	junc = next_junc;
end

%% Create Stateflow for an if statement
%Based on the AST, creates the if condition
% Inputs:
%   - flw_chrt: The flow chart function that will be added two
%   - curr_junc: The junction that will be built off of
%   - node: The current node of the AST
%
%Outputs:
%   - The last transition of the state chart (returns [] for if condition)
%   - the last junction of the state chart
function [trans,junc] = createIfStatement(flw_chrt, prev_trans, curr_junc, node)

	%Creating the logic for if the statement is true
	dest_junc_true = StChrt_generateJunction(flw_chrt, node, curr_junc, true);
	condition = ['[' createCondition(node.cond) ']'];
	trans_true = StChrt_generateTransition(flw_chrt, curr_junc,dest_junc_true, condition,true);
	[trans_true, dest_junc_true] = createFunctionBody(...
		flw_chrt, trans_true, dest_junc_true, node.iftrue);

	%Creating the logic for if the statement is false
	dest_junc_false = StChrt_generateJunction(flw_chrt, 0, curr_junc, false) ;
	trans_false =  StChrt_generateTransition(flw_chrt, curr_junc,dest_junc_false, '', false);
	[trans_false, dest_junc_false] = createFunctionBody(...
		flw_chrt, trans_false, dest_junc_false, node.iffalse);


	%Destination junction for after the if statement.
	if dest_junc_false.Position.Center(2) < dest_junc_true.Position.Center(2)
		if isempty (trans_false)
			trans_false = StChrt_generateTransition(flw_chrt, dest_junc_false,dest_junc_true, '', false);
		else
			StChrt_changeTransitionDest(trans_false, dest_junc_false,dest_junc_true);
		end
		junc = dest_junc_true;
	else
		if isempty (trans_true)
			trans_true = StChrt_generateTransition(flw_chrt, dest_junc_true,dest_junc_false, '', false);
		else
			StChrt_changeTransitionDest(trans_true, dest_junc_true, dest_junc_false);
		end
		junc = dest_junc_false;
	end

	%%trans_final = StChrt_generateTransition(flw_chrt, src_junc,dest_junc, '');
	trans = [];

end


%% Create Stateflow for an if statement
%Based on the AST, creates the if condition
% Inputs:
%   - flw_chrt: The flow chart function that will be added two
%   - curr_junc: The junction that will be built off of
%   - node: The current node of the AST
%
%Outputs:
%   - The last transition of the state chart (returns [] for if condition)
%   - the last junction of the state chart
function [trans,junc] = createSwitchStatement(flw_chrt, prev_trans, curr_junc, node)


	% create final node
	dest_junc_final = StChrt_generateSwitchJunction(flw_chrt, 0, curr_junc, false, 0, length(node.stmt.block_items)) ;

	% loop over each case in the switch
	for i = 1:1:length(node.stmt.block_items)
		stmt = node.stmt.block_items(i);

		dest_junc_true = StChrt_generateSwitchJunction(flw_chrt, node, curr_junc, true,  i, length(node.stmt.block_items));

		%Creating the logic for if the statement is true
		condition = ['[' createCondition(node.cond) ' == ' stmt.expr.value ']'];
		trans_true = StChrt_generateTransition(flw_chrt, curr_junc, dest_junc_true, condition,true);
		[trans_true, dest_junc_true] = createFunctionBody(...
			flw_chrt, trans_true, dest_junc_true, stmt.stmts);


		if (i<length(node.stmt.block_items))
			%Creating the logic for if the statement is false
			dest_junc_false = StChrt_generateJunction(flw_chrt, 0, curr_junc, false) ;
			trans_false =  StChrt_generateTransition(flw_chrt, curr_junc,dest_junc_false, '', false);
			curr_junc = dest_junc_false;
		else
			trans_false =  StChrt_generateTransition(flw_chrt, curr_junc,dest_junc_final, '', false);
		end


		StChrt_generateTransition(flw_chrt, dest_junc_true,dest_junc_final, '', false);

	end

	trans = [];
	junc = dest_junc_final;

end



%% Create a Transition for a statement
% Inputs:
%   - flw_chrt: The flow chart function that will be added two
%   - curr_junc: The junction that will be built off of
%   - statement: The action for the transition
%
%Outputs:
%   - the last junction of the state chart
function [trans, junc] = createStatementTransition(flw_chrt, prev_trans, curr_junc, statement)
	%if the last transition only had a condition, then
	%add the Declarations as the actions to that
	%transition, else make a new transition
	if ~isempty(prev_trans)
		prev_trans_str = prev_trans.LabelString;
		last_char = extractAfter(prev_trans_str,length(prev_trans_str)-1);

		% if there and we check to see if there is a previous
		% statement if so, add a \
		prev_trans_str = strrep(prev_trans_str,'?','');
		if ~strcmp(prev_trans_str,'')
			prev_trans_str = [prev_trans_str  newline];
		end
	end

	if ~isempty(prev_trans) && ~strcmp(last_char, ']')
		if  strcmp(last_char, '}')
			prev_trans_str = prev_trans_str(1:length(prev_trans_str)-2);
		elseif strcmp(last_char, ']')
			prev_trans_str = [prev_trans_str newline '{' newline ];
		elseif strcmp(last_char, '?')
			prev_trans_str = ['{' newline ];
		end
		prev_trans_str = [prev_trans_str statement];
		prev_trans.LabelString = [prev_trans_str newline '}'];
		trans = prev_trans;
		junc = curr_junc;
	else
		junc = StChrt_generateJunction(flw_chrt, 0, curr_junc, false);
		if ~isempty(statement) && ~strcmp(statement, '') && ~strcmp(erase(statement,newline), '{}')
			trans = StChrt_generateTransition(flw_chrt, curr_junc,junc, ['{' newline statement  '}'], false);
		else
			trans = StChrt_generateTransition(flw_chrt, curr_junc,junc, '', false);
		end

	end
end

%% Get the value that will be assigned by a Declaration
%
% Input:
%   - node: The node of the value
% Output:
%   - value: The assignment expression
function  value = getAssignmentExp(node)

	if strcmp(node.x_nodetype,'Constant')
		value = node.value;
	elseif strcmp(node.x_nodetype,'StructRef')
		%NOTE since pointers are not allowed, any pointers are assumed to
		%be direct accesses
		value = [ createCondition(node.name) '.' createCondition(node.field)];
	elseif strcmp(node.x_nodetype,'ID')
		value = node.name;
	elseif strcmp(node.x_nodetype,'BinaryOp')
		value = [getAssignmentExp(node.left) node.op ...
			getAssignmentExp(node.right)];
	elseif strcmp(node.x_nodetype, 'UnaryOp')
		if strcmp(node.op, 'p--') || strcmp(node.op, 'p++')
			value = [createCondition(node.expr) node.op];
		elseif strcmp(node.op, '--') || strcmp(node.op, '++')
			value = [node.op createCondition(node.expr)];
		elseif strcmp(node.op, '-') || strcmp(node.op, '+')
			value = [node.op createCondition(node.expr)];
		elseif strcmp(node.op, '!')
			value = ['~(' createCondition(node.expr) ')'];
		else
			error(['Unsupported Unary Operator: ' node.x_nodetype]);
		end

	elseif strcmp(node.x_nodetype,'Cast')
		value = getAssignmentExp(node.expr);
	elseif strcmp(node.x_nodetype,'FuncCall')
		value = getFunctionDef(node);
	else
		error(['Unsupported assignment expression: ' node.x_nodetype]);
	end

end


%% Create the condition expression
%
% Input
%   - cond: The condition AST object
% Ouputs
%   - the condition expression in a string format

function condition = createCondition(cond)

	if strcmp(cond.x_nodetype,'BinaryOp')
		condition = [createCondition(cond.left) cond.op ...
			createCondition(cond.right)] ;
	elseif strcmp(cond.x_nodetype,'StructRef')
		%NOTE since pointers are not allowed, any pointers are assumed to
		%be direct accesses
		condition = [createCondition(cond.name) '.' createCondition(cond.field)];
	elseif strcmp(cond.x_nodetype,'ID')
		condition = cond.name;
	elseif strcmp(cond.x_nodetype, 'Constant')
		condition = cond.value;
	elseif strcmp(cond.x_nodetype,'UnaryOp')
		if strcmp(cond.op, 'p--') || strcmp(cond.op, 'p++')
			condition = [createCondition(cond.expr) cond.op];
		elseif strcmp(cond.op, '--') || strcmp(cond.op, '++')
			condition = [cond.op createCondition(cond.expr)];
		elseif strcmp(cond.op, '!')
			condition = ['~(' createCondition(cond.expr) ')'];
		else
			error(['Unsupported Unary Operator: ' cond.x_nodetype]);
		end
	elseif  strcmp(cond.x_nodetype, 'FuncCall')
		condition = getFunctionDef(cond);
	else
		error(['Unsupported boolean expression: ' cond.x_nodetype]);
	end

end

%% Build the variable name
%
% Input
%   - node: The condition AST object
% Ouputs
%   - the name of the variable in a string format

function name = buildVariableName(node)
	if strcmp(node.x_nodetype,'StructRef')
		%NOTE since pointers are not allowed, any pointers are assumed to
		%be direct accesses
		name = [buildVariableName(node.name) '.' buildVariableName(node.field)];
	elseif strcmp(node.x_nodetype,'ID')
		name = node.name;
	elseif strcmp(node.x_nodetype,'Constant')
		name = node.value;
	else
		error(['Unsupported variable: ' node.x_nodetype]);
	end
	end

	%% Get Declaration String
	%
	% Inputs
	%   - node: The current AST node
	%
	%Outputs:
	%   - decl: The string corresponding to the Declaration
	function decl = getDeclaration(node)
	decl = [node.name ' = ' getAssignmentExp(node.init) ';' newline];
	end

	%% Get Assignment String
	%
	% Inputs
	%   - node: The current AST node
	%
	%Outputs:
	%   - assgn: The string corresponding to the assignment
	function assgn = getAssignment(node)
	assgn = [buildVariableName(node.lvalue), ' = ', getAssignmentExp(node.rvalue) ';' newline];
end


%% Get Function Definition String
%
% Inputs
%   - node: The current AST node
%
%Outputs:
%   - func: The string corresponding to the funciton
function func = getFunctionDef(node)
	if 0 < length(node.args)
		arg_strs = ['(' getArgument(node.args.exprs(1))];
		for i =2:1:length(node.args.exprs)
			arg_strs = [arg_strs ', ' getArgument(node.args.exprs(i))];

		end
		arg_strs = [arg_strs ')'];
	else
		arg_strs = '()';
	end

	func = [node.name.name arg_strs ];
end

%% Get the argument to the function
%
% Inputs
%   - node: The current AST node
%
%Outputs:
%   - func: The string corresponding to the funciton
function value = getArgument(node)
	if strcmp(node.x_nodetype, 'ID')
		value = node.name;
	elseif strcmp(node.x_nodetype, 'Constant')
		value = node.value;
	elseif strcmp(node.x_nodetype, 'StructRef')
		value = [buildVariableName(node.name) '.' buildVariableName(node.field)];
	elseif strcmp(node.x_nodetype, 'BinaryOp')
		value = [getAssignmentExp(node.left) node.op getAssignmentExp(node.right)];
	else
		error(['Unsupported variable: ' node.x_nodetype]);
	end
end


function junc = StChrt_generateJunction(flw_chrt, node, prev_junc, isTrueBranch)
	junc = Stateflow.Junction(flw_chrt);
	delta = 200;
	if isTrueBranch
		width = getNestedDepth(node.iffalse)+1;
		junc.Position.Center =[prev_junc.Position.Center(1)+delta*width, ...
			prev_junc.Position.Center(2)];
	else
		junc.Position.Center =[prev_junc.Position.Center(1), ...
			prev_junc.Position.Center(2)+delta];
	end
end

function junc = StChrt_generateSwitchJunction(flw_chrt, node, prev_junc, isTrueBranch, index, num_cases)
	junc = Stateflow.Junction(flw_chrt);
	delta = 200;
	if isTrueBranch
		junc.Position.Center =[prev_junc.Position.Center(1)+delta*(num_cases-(index-1) - (index-num_cases)*.2), ...
			prev_junc.Position.Center(2)+delta*(index-1+(index-1)*.2)];
	else
		junc.Position.Center =[prev_junc.Position.Center(1), ...
			prev_junc.Position.Center(2)+delta*(num_cases-1+(num_cases-1)*.2)];
	end
end

function trans = StChrt_generateTransition(flw_chrt, sourceNode,destNode, statement, condBranch)
	trans = Stateflow.Transition(flw_chrt);
	trans.Source = sourceNode;
	trans.Destination = destNode;
	trans.LabelString = statement;
	xsource = (sourceNode.Position.Center(1) + destNode.Position.Center(1))/2.0;
	ysource = (sourceNode.Position.Center(2) + destNode.Position.Center(2))/2.0;
	trans.MidPoint = [xsource ysource];
	if condBranch
		trans.LabelPosition(1) = sourceNode.Position.Center(1)+10;
		trans.LabelPosition(2) = ysource-20;
	else
		trans.LabelPosition(1) = xsource+15;
		trans.LabelPosition(2) = ysource;
	end

	end


function trans = StChrt_changeTransitionDest(trans, oldDest,newDest)

	x = trans.Source.Position.Center(1);
	oldDest.delete;
	trans.Destination = newDest;

	xsource = (trans.Source.Position.Center(1) + newDest.Position.Center(1))/2.0;
	ysource = (trans.Source.Position.Center(2) + newDest.Position.Center(2))/2.0;

	%trans.MidPoint = [xsource ysource];
	trans.LabelPosition(1) = xsource+15;
	trans.LabelPosition(2) = ysource;

	end



function number = getNestedDepth(node)
	if isempty(node)
		number = 0;
	else
		switch node.x_nodetype
			case 'Compound'
				number = -1;
				blcks = node.block_items;
				for i = 1:1:length(blcks)
					if iscell(blcks)
						number = max(number, getNestedDepth(blcks{i}));
					else
						number = max(number, getNestedDepth(blcks(i)));
					end
				end
			case 'Decl'
				number = 0;
			case 'Assignment'
				number = 0;
			case 'FuncCall'
				number = 0;
			case 'Return'
				number = 0;
			case 'UnaryOp'
				number = 0;
			case 'If'
				num_true = getNestedDepth(node.iftrue);
				num_false = getNestedDepth(node.iffalse);
				number = num_true+num_false+1;
			case 'Switch'
				num_true = getNestedDepth(node.stmt);
				number = num_true+1;
			case 'Case'
				num_true = getNestedDepth(node.stmts);
				number = num_true+1;
			otherwise
				error(['Unsupported C Syntax: ' node.x_nodetype]);
		end
	end
end



