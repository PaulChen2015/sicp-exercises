from sicp.lisplib import *
def evals(exp, env):
    if is_self_evaluating(exp):
        return exp
    elif is_variable(exp):
        return lookup_variable_value(exp, env)
    elif is_quoted(exp):
        return text_of_quotation(exp)
    elif is_assignment(exp):
        return eval_assignment(exp, env)
    elif is_definition(exp):
        return eval_definition(exp, env)
    elif is_if(exp):
        return eval_if(exp, env)
    elif is_lambda(exp):
        return make_procedure(lambda_parameters(exp), lambda_body(exp), env)
    elif is_begin(exp):
        return eval_sequence(begin_actions(exp), env)
    elif is_cond(exp):
        return evals(cond_to_if(exp), env)
    elif is_application(exp):
        return apply(evals(operator(exp), env), list_of_values(operands(exp), env))
    else:
        raise Exception("Unknow expression type:EVAL", exp)



def apply(procedure, arguments):
    if is_primitive_procedure(procedure):
        return apply_primitive_procedure(procedure, arguments)
    elif compound_procedure(procedure):
        return eval_sequence(procedure_body(procedure),
                                 extend_environment(procedure_arguments(procedure),
                                                    arguments,
                                                    procedure_environment(procedure)))
    else:
        raise Exception("Unknown procedure type: APPLY", procedure)

def list_of_values(exps, env):
    if no_operands(exps):
        return None
    else:
        return cons(evals(first_operand(exps), env),
                    list_of_values(rest_operands(exps), env))

def eval_if(exp, env):
    if is_true(evals(if_predicate(exp), env)):
        return evals(if_consequent(exp), env)
    else:
        return evals(if_alternative(exp), env)

def eval_sequence(exps, env):
    if is_last_exp(exps):
        return evals(first_exp(exps), env)
    else:
        evals(first_exp(exps), env)
        return eval_sequence(rest_exps(exps), env)

def eval_assignment(exp, env):
    set_variable_value(assignment_variable(exp),
                       evals(assignment_value(exp), env),
                       env)
    print("ok")

def eval_definition(exp, env):
    define_variable(definition_variable(exp),
                    evals(definition_value(exp), env),
                    env)
    print("ok")

# how string number symbol pair means in python
def is_number(exp):pass
def is_string(exp):pass
def is_symbol(exp):pass
def is_pair(exp):pass

def is_self_evaluating(exp):
    return is_number(exp) or is_string(exp)

def is_variable(exp):
    return is_symbol(exp)

def is_quoted(exp):
    return is_tagged_list(exp, "quote")

def text_of_quotation(exp):
    return cadr(exp)

def is_tagged_list(exp, tag):
    if is_pair(exp):
        return car(exp) == tag
    return False

def is_assignment(exp):
    return is_tagged_list(exp, "set!")

def assignment_variable(exp):
    return cadr(exp)

def assignment_value(exp):
    return caddr(exp)

def is_definition(exp):
    return is_tagged_list(exp, "define")

def definition_variable(exp):
    if is_symbol(cadr(exp)):
        return cadr(exp)
    else:
        return caadr(exp)

def definition_value(exp):
    if is_symbol(cadr(exp)):
        return caddr(exp)
    else:
        return make_lambda(cdadr(exp), cddr(exp))

def is_lambda(exp):
    return is_tagged_list(exp, "lambda")

def lambda_parameters(exp):
    return cadr(exp)

def lambda_body(exp):
    return cddr(exp)

def make_lambda(parameters, body):
    return cons("lambda", cons(parameters, body))

def is_if(exp):
    return is_tagged_list(exp, "if")

def if_predicate(exp):
    return cadr(exp)

def if_consequent(exp):
    return caddr(exp)

def if_alternative(exp):
    if cdddr(exp) is not None:
        return cadddr(exp)
    else:
        return "false"

def lists(*args):pass

def make_if(predicate, consequent, alternative):
    return lists("if", predicate, consequent, alternative)

def is_begin(exp):
    return is_tagged_list(exp, "begin")

def begin_actions(exp):
    return cdr(exp)

def is_last_exp(seq):
    return cdr(seq) is None

def first_exp(seq):
    return car(seq)

def rest_exps(seq):
    return cdr(seq)

def sequence_to_exp(seq):
    if seq is None:
        return seq
    elif is_last_exp(seq):
        return first_exp(seq)
    else:
        return make_begin(seq)

def make_begin(seq):
    return cons("begin", seq)

def is_application(exp):
    return is_pair(exp)

def operator(exp):
    return car(exp)

def operands(exp):
    return cdr(exp)

def is_no_operands(ops):
    return ops is None

def first_operand(ops):
    return car(ops)

def rest_operands(ops):
    return cdr(ops)

def is_cond(exp):
    return is_tagged_list(exp, "cond")

def cond_clauses(exp):
    return cdr(exp)

def is_cond_else_clause(clause):
    return cond_predicate(clause) == "else"

def cond_predicate(clause):
    return car(clause)

def cond_actions(clause):
    return cdr(clause)

def cond_to_if(exp):
    return expand_clauses(cond_clauses(exp))

def expand_clauses(clauses):
    if clauses is None:
        return "false"
    else:
        first = car(clauses)
        rest = cdr(clauses)
        if is_cond_else_clause(first):
            if rest is None:
                return sequence_to_exp(cond_actions(first))
            else:
                raise Exception("ELSE clause isn't last: COND->IF", clauses)
        else:
            return make_if(cond_predicate(first),
                           sequence_to_exp(cond_actions(first)),
                           expand_clauses(rest))

def is_true(x):
    return not (x == "false")

def is_false(x):
    return x == "false"

def make_procedure(parameters, body, env):
    return lists("procedure", parameters, body, env)

def is_compound_procedure(p):
    return is_tagged_list(p, "procedure")

def procedure_parameters(p):
    return cadr(p)

def procedure_body(p):
    return caddr(p)

def procedure_environment(p):
    return cadddr(p)

def enclosing_environment(env):
    return cdr(env)

def first_frame(env):
    return car(env)

the_empty_environment = "()"

def make_frame(variables, values):
    return cons(variables, values)

def frame_variables(frame):
    return car(frame)

def frame_values(frame):
    return cdr(frame)

def add_binding_to_frame(var, val, frame):
    frame = cons(cons(var, car(frame)),
                 cons(val, cdr(frame)))

def length(s):pass

def extend_environment(varis, vals, base_env):
    if length(varis) == length(vals):
        return cons(make_frame(varis, vals), base_env)
    elif length(varis) < length(vals):
        raise Exception("Too many arguments supplied", varis, vals)
    else:
        raise Exception("Too few arguments supplied", varis, vals)

def lookup_variable_value(var, env):
    def env_loop(env):
        def scan(varis, vals):
            if varis is None:
                return env_loop(enclosing_environment(env))
            elif var == car(varis):
                return car(vals)
            else:
                return scan(cdr(varis), cdr(vals))
        if env == the_empty_environment:
            raise Exception("Unbound variable", var)
        else:
            frame = first_frame(env)
            return scan(frame_variables(frame), frame_values(frame))
    return env_loop(env)
        
def set_varialbe_value(var, val, env):
    def env_loop(env):
        def scan(varis, vals):
            if varis is None:
                return env_loop(enclosing_environment(env))
            elif var == car(varis):
                varis = cons(val, cdr(varis))
            else:
                return scan(cdr(varis), cdr(vals))
        if env == the_empty_environment:
            raise Exception("Unbound variable: SET!", var)
        else:
            frame = first_frame(env)
            return scan(frame_variables(frame), frame_values(frame))
    return env_loop(env)

def define_variable(var, val, env):
    frame = first_frame(env)
    def scan(varis, vals):
        if varis is None:
            add_binding_to_frame(var, val, frame)
        elif var == car(varis):
            varis = cons(val, cdr(varis))
        else:
            return scan(cdr(varis), cdr(vals))
    return scan(frame_variables(frame), frame_values(frame))


def is_primitive_procedure(proc):
    return is_tagged_list(proc, "primitive")

def primitive_implementation(proc):
    return cadr(proc)

def is_null(a):
    return a is None

primitive_procedures = lists(lists("car", car),
                             lists("cdr", cdr),
                             lists("cons", cons),
                             lists("is_null", is_null))

primitive_procedure_names = lisp_map(car, primitive_procedures)

primitive_procedure_objects = lisp_map((lambda proc: lists("primitive", cadr(proc))),
                                        primitive_procedures)

def setup_environment():
    initial_env = extend_environment(primitive_procedure_names,
                                     primitive_procedure_objects,
                                     the_empty_environment)
    define_variable("true", True, initial_env)
    define_variable("false", False, initial_env)

the_global_environment = setup_environment()

apply_in_underlying_scheme = apply

def apply_primitive_procedure(proc, args):
    return apply_in_underlying_scheme(primitive_implementation(proc), args)

input_prompt = ";;; M-Eval input:"
output_prompt = ";;; M-Eval value:"

def count_char(strs, char):
    count = 0
    for c in strs:
        if c == char:
            count += 1
    return count

def driver_loop():
    prompt_for_input(input_prompt)
    inputs = input()
    while count_char(inputs, '(') != count_char(inputs, ')'):
        inputs += " " + input()
    output = evals(inputs.split(), the_global_environment)
    announce_output(output_prompt)
    user_print(output)
    return driver_loop()

def prompt_for_input(string):
    print("\n\n", string)

def announce_output(string):
    print("\n", string)

def user_print(obj):
    if is_compound_procedure(obj):
        print(lists("compound_procedure",
                    procedure_parameters(obj),
                    procedure_body(obj),
                    "<procedure_env>"))
    else:
        print(obj)
