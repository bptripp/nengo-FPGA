#include <stdio.h>
#include <pcap.h>

#define ETHER_ADDR_LEN 6
#define SIZE_ETHERNET 14
#define SIZE_BOARDDATA 5

struct pkt_ethernet {
  u_char ether_dhost[ETHER_ADDR_LEN];
  u_char ether_shost[ETHER_ADDR_LEN];
  u_short ether_type;
};
struct pkt_boarddata {
  u_char board_tag;
  u_char board_op;
  u_short board_addr;
  u_char board_paircount;
};

void process_packet(u_char *args, const struct pcap_pkthdr *header, const u_char *packet){
  const struct pkt_ethernet *ethernet;
  const struct pkt_boarddata *boarddata;
  const u_char *payload;
  ethernet = (struct pkt_ethernet*)(packet);
  boarddata = (struct pkt_boarddata*)(packet + SIZE_ETHERNET);
  payload = (u_char*)(packet + SIZE_ETHERNET + SIZE_BOARDDATA);

  unsigned int count = boarddata->board_paircount + 1;
  unsigned int i;
  for(i = 0; i < count; ++i){
    printf("%.2x%.2x%.2x", (unsigned int)payload[0], (unsigned int)payload[1], (unsigned int)payload[2]);
  }
  printf("\n");
}

int main(int argc, char *argv[]){
  char errbuf[PCAP_ERRBUF_SIZE];
  pcap_t *handle;
  struct bpf_program fp;
  char filter_exp[] = "ether broadcast";

  struct pcap_pkthdr header;
  const u_char *packet;

  if(argc != 2){
    fprintf(stderr, "Usage: %s pcapng-file\n", argv[0]);
    return 1;
  }
  handle = pcap_open_offline(argv[1], errbuf);
  if(handle == NULL){
    fprintf(stderr, "Couldn't open capture file: %s\n", errbuf);
    return 1;
  }
  if(pcap_datalink(handle) != DLT_EN10MB){
    fprintf(stderr, "No Ethernet headers found\n");
    return 1;
  }
  if(pcap_compile(handle, &fp, filter_exp, 0, PCAP_NETMASK_UNKNOWN) == -1){
    fprintf(stderr, "Couldn't parse filter '%s': %s\n", filter_exp, pcap_geterr(handle));
    return 1;
  }
  if(pcap_setfilter(handle, &fp) == -1){
    fprintf(stderr, "Couldn't install filter '%s': %s\n", filter_exp, pcap_geterr(handle));
    return 1;
  }

  pcap_loop(handle, -1, process_packet, NULL);
  pcap_close(handle);
  return 0;
}
