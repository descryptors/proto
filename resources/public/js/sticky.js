//** STICKY DOTS
let alignDots = () => {
  let currItem, prevItem, idx;
  let items = document.getElementsByClassName("coin");
  for (let i = 0; i < items.length; i++) {
    items[ i ].classList.remove('right-coin');
    currItem = items[ i ].getBoundingClientRect();
    if (prevItem && prevItem.top < currItem.top) break;
    prevItem = currItem;
    idx = i;
  }

  if (idx >= 0) {
    items[ idx ].classList.add('right-coin');
  }
};


let isChrome = !!window.chrome && (!!window.chrome.webstore || !!window.chrome.runtime);
let isFirefox = typeof InstallTrigger !== 'undefined';
