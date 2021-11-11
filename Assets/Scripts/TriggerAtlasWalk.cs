using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class TriggerEntrance : MonoBehaviour
{
    [SerializeField] public Animator atlasParent;


    private void OnTriggerEnter(Collider other)
    {
        if(other.CompareTag("Player"))
        {
            atlasParent.SetTrigger("EnteredVolume2");
        }
    }
}
