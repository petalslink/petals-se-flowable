/**
 * Copyright (c) 2017 Linagora
 * 
 * This program/library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or (at your
 * option) any later version.
 * 
 * This program/library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program/library; If not, see http://www.gnu.org/licenses/
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.flowable;

import javax.jbi.messaging.ExchangeStatus;
import javax.xml.transform.Source;

import org.junit.Test;
import org.ow2.easywsdl.wsdl.api.abstractItf.AbsItfOperation;
import org.ow2.petals.component.framework.junit.ResponseMessage;
import org.ow2.petals.component.framework.junit.StatusMessage;
import org.ow2.petals.component.framework.junit.impl.message.RequestToProviderMessage;
import org.ow2.petals.se_flowable.unit_test.intermediate_message_catch_event.Start;
import org.ow2.petals.se_flowable.unit_test.intermediate_message_catch_event.StartResponse;
import org.ow2.petals.se_flowable.unit_test.intermediate_message_catch_event.Unlock;

import com.ebmwebsourcing.easycommons.xml.SourceHelper;

/**
 * Unit tests about request processing of BPMN services, with a component configured with default values and a process
 * with call a BPMN element 'intermediate message catch event'
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class IntermediateMessageCatchEventProcessTest extends IntermediateMessageCatchEventProcessTestEnvironment {

    private static final String ERROR_MSG = "A dummy error occurs";

    private static final String CUSTOMER_ADRESS = "customer adress";

    /**
     * <p>
     * Check the message processing where: a valid request is sent to event receipt.
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>the process instance is correctly created in the Flowable engine,</li>
     * <li>the process instance is automatically completed when the event is received.</li>
     * </ul>
     * </p>
     */
    @Test
    public void nominal() throws Exception {

        // Create a new instance of the process definition
        final StringBuilder processInstance = new StringBuilder();
        {
            final Start start = new Start();
            start.setCustomer(BPMN_USER);

            // Send the 1st valid request for start event 'request
            final RequestToProviderMessage request_1 = new RequestToProviderMessage(COMPONENT_UNDER_TEST,
                    INTERMEDIATE_MESSAGE_CATCH_EVENT_SU, OPERATION_START,
                    AbsItfOperation.MEPPatternConstants.IN_OUT.value(),
                    toByteArray(start));
            COMPONENT_UNDER_TEST.pushRequestToProvider(request_1);
            final ResponseMessage response_1 = COMPONENT_UNDER_TEST.pollResponseFromProvider();
            final Source fault = response_1.getFault();
            assertNull("Unexpected fault", (fault == null ? null : SourceHelper.toString(fault)));
            assertNotNull("No XML payload in response", response_1.getPayload());
            final Object responseObj = UNMARSHALLER.unmarshal(response_1.getPayload());
            assertTrue(responseObj instanceof StartResponse);
            final StartResponse response = (StartResponse) responseObj;
            assertNotNull(response.getCaseFileNumber());
            processInstance.append(response.getCaseFileNumber());
        }

        assertProcessInstancePending(processInstance.toString(), BPMN_PROCESS_DEFINITION_KEY);

        // Send the event to the process instance
        {
            final Unlock unlockRequest = new Unlock();
            unlockRequest.setInstanceId(processInstance.toString());

            final RequestToProviderMessage request_2 = new RequestToProviderMessage(COMPONENT_UNDER_TEST,
                    INTERMEDIATE_MESSAGE_CATCH_EVENT_SU, OPERATION_UNLOCK,
                    AbsItfOperation.MEPPatternConstants.ROBUST_IN_ONLY.value(), toByteArray(unlockRequest));
            COMPONENT_UNDER_TEST.pushRequestToProvider(request_2);
            final StatusMessage response_2 = COMPONENT_UNDER_TEST.pollStatusFromProvider();
            assertEquals(ExchangeStatus.DONE, response_2.getStatus());
        }

        // Wait the end of the process instance
        waitEndOfProcessInstance(processInstance.toString());

        // Assertions about state of process instance at Flowable Level
        assertProcessInstanceFinished(processInstance.toString());

        // Check MONIT traces
        // TODO: To do when MONIT traces will be defined

    }
}
