fetch("https://a.dpitt.me/visit", {
  method: "POST",
  headers: { "Content-Type": "application/json" },
  body: JSON.stringify({
    pageRoute: location.pathname,
    pageTitle: document.title,
    pageReferrer: document.referrer,
  }),
  keepalive: true,
});
