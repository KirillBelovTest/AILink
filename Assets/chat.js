const makeMessage = (el, newMessages) => {
  newMessages.forEach(msg => {
    const messageElement = document.createElement('div');
    messageElement.classList.add('chat-message', 'chat-'+msg.role);
    const span = document.createElement('span');
    span.innerText = msg.content;
    messageElement.appendChild(span);
    //const el = env.element.firstChild.firstChild;
    el.appendChild(messageElement);
    el.scrollBy(0, el.scrollHeight, {behavior: "smooth"});
  });
}

core.ChatView = async (args, env) => {
  const messages = await interpretate(args[0], env);
  const channel  = await interpretate(args[1], env);

  env.local.length = messages.length; //local memory of the instance
  env.element.innerHTML = `<div class="chat sm-controls cursor-default rounded-md 0 py-2 pl-3 bg-gray-100 pr-2 text-gray-500 ring-1 ring-inset ring-gray-400 text-xs"><div class="flex flex-col text-left overflow-y-scroll max-h-60 p-2"></div><div class="overflow-hidden bg-gray-100 px-2 rounded-lg ring-1 ring-inset dark:ring-gray-800 dark:bg-gray-700 ring-gray-300 focus-within:ring-2 focus-within:ring-teal-600" style="padding-right:6.5rem; z-index:100;"><form><input type="text" class="text-sm  block w-full resize-none border-0 bg-transparent py-1.5 dark:text-gray-400 text-gray-900 placeholder:text-gray-400 focus:ring-0 sm:text-sm sm:leading-6" placeholder="Add your comment..."/><input type="submit" hidden /></form></div></div>`; 

  const inputField = env.element.getElementsByTagName('input')[0];
  const form = env.element.getElementsByTagName('form')[0];

  form.addEventListener('submit', (e) => {
    e.preventDefault();
    const value = inputField.value.trim();
    if (value.length < 1) return;
    console.log(value);
    makeMessage(env.element.firstChild.firstChild, [{role:'user', content: value}]);
    env.local.length = env.local.length + 1;
    inputField.value = '';

    env.local.animation = {
      remove: () => {
        env.element.firstChild.firstChild.style.animation = '';
        delete env.local.animation;
      }
    };
    
    env.element.firstChild.firstChild.style.animation = 'hue-animation 1s infinite';

    server.kernel.emitt(channel, '"'+encodeURIComponent(value)+'"');
    
  });
  
  makeMessage(env.element.firstChild.firstChild, messages);
}

core.ChatView.update = async (args, env) => {
  const messages = await interpretate(args[0], env);

  const newMessages = messages.slice(env.local.length);
  env.local.length = messages.length;

  if (env.local.animation) env.local.animation.remove();
  
  makeMessage(env.element.firstChild.firstChild, newMessages);
}

core.ChatView.destroy = () => {}

core.ChatView.virtual = true //enable instancing and coupling
