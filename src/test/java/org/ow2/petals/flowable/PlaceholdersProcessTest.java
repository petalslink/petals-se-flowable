/**
 * Copyright (c) 2017-2020 Linagora
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
import javax.jbi.messaging.MessageExchange;
import javax.xml.transform.Source;

import org.junit.Test;
import org.ow2.easywsdl.wsdl.api.abstractItf.AbsItfOperation;
import org.ow2.petals.component.framework.junit.Message;
import org.ow2.petals.component.framework.junit.RequestMessage;
import org.ow2.petals.component.framework.junit.StatusMessage;
import org.ow2.petals.component.framework.junit.helpers.MessageChecks;
import org.ow2.petals.component.framework.junit.helpers.ServiceProviderImplementation;
import org.ow2.petals.component.framework.junit.impl.message.RequestToProviderMessage;
import org.ow2.petals.component.framework.junit.impl.message.ResponseToConsumerMessage;
import org.ow2.petals.se_flowable.unit_test.placeholders.Start;
import org.ow2.petals.se_flowable.unit_test.placeholders.StartResponse;
import org.ow2.petals.se_flowable.unit_test.placeholders.Unlock;
import org.ow2.petals.se_flowable.unit_test.placeholders.UnlockAck;
import org.ow2.petals.se_flowable.unit_test.placeholders.archivageservice.Archiver;
import org.ow2.petals.se_flowable.unit_test.placeholders.archivageservice.ArchiverResponse;

import com.ebmwebsourcing.easycommons.xml.SourceHelper;

/**
 * Unit tests about request processing of BPMN services using placeholders
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class PlaceholdersProcessTest extends PlaceholdersProcessTestEnvironment {

    private final String PLACEHOLDER_VALUE_1 = "placeholder-value-1";

    private final String PLACEHOLDER_VALUE_2 = "placeholder-value-2";

    /**
     * <p>
     * Check the message processing where: a valid request is sent to process using placeholders correctly defined
     * </p>
     * <p>
     * Expected results:
     * </p>
     * <ul>
     * <li>the process instance is correctly created in the Flowable engine,</li>
     * <li>the placeholders are correctly used in main process,</li>
     * <li>the placeholders are correctly used in main sub-process,</li>
     * <li>the process instance is automatically completed when all is correctly done.</li>
     * </ul>
     */
    @Test
    public void execute() throws Exception {

        // --------------------------------------------------------------------------
        // Create a new instance of the process definition:
        // - When creating the process instance the service 'Archive' is called
        // - After creation, the process instance is waiting a user task completion
        // --------------------------------------------------------------------------

        // Create a new instance of the process definition
        final StringBuilder processInstance = new StringBuilder();
        {
            final Start start = new Start();
            start.setCustomer(BPMN_USER);

            final RequestToProviderMessage request = new RequestToProviderMessage(COMPONENT_UNDER_TEST,
                    PLACEHOLDERS_SU, OPERATION_START, AbsItfOperation.MEPPatternConstants.IN_OUT.value(),
                    toByteArray(start));

            COMPONENT.sendAndCheckResponseAndSendStatus(request, new ServiceProviderImplementation() {
                private MessageExchange msgExchange;

                @Override
                public Message provides(final RequestMessage requestMsg) throws Exception {

                    this.msgExchange = requestMsg.getMessageExchange();
                    assertNotNull(this.msgExchange);
                    assertEquals(ARCHIVE_INTERFACE, this.msgExchange.getInterfaceName());
                    assertEquals(ARCHIVE_SERVICE, this.msgExchange.getService());
                    assertNotNull(this.msgExchange.getEndpoint());
                    assertEquals(ARCHIVE_ENDPOINT, this.msgExchange.getEndpoint().getEndpointName());
                    assertEquals(ARCHIVER_OPERATION, this.msgExchange.getOperation());
                    assertEquals(this.msgExchange.getStatus(), ExchangeStatus.ACTIVE);
                    final Object requestObj = UNMARSHALLER.unmarshal(requestMsg.getPayload());
                    assertTrue(requestObj instanceof Archiver);
                    assertEquals(PLACEHOLDER_VALUE_1, ((Archiver) requestObj).getItem2());

                    // Returns the reply of the service provider to the Flowable service task
                    return new ResponseToConsumerMessage(requestMsg, toByteArray(new ArchiverResponse()));
                }

                @Override
                public void handleStatus(final StatusMessage statusDoneMsg) throws Exception {
                    // Assert the status DONE on the message exchange
                    assertNotNull(statusDoneMsg);
                    // It's the same message exchange instance
                    assertSame(statusDoneMsg.getMessageExchange(), this.msgExchange);
                    assertEquals(statusDoneMsg.getMessageExchange().getStatus(), ExchangeStatus.DONE);
                }
            }, new MessageChecks() {

                @Override
                public void checks(final Message message) throws Exception {
                    // Check the reply
                    final Source fault = message.getFault();
                    assertNull("Unexpected fault", (fault == null ? null : SourceHelper.toString(fault)));
                    assertNotNull("No XML payload in response", message.getPayload());
                    final Object responseObj = UNMARSHALLER.unmarshal(message.getPayload());
                    assertTrue(responseObj instanceof StartResponse);
                    final StartResponse response = (StartResponse) responseObj;
                    assertNotNull(response.getCaseFileNumber());
                    processInstance.append(response.getCaseFileNumber());
                }
            }, ExchangeStatus.DONE);
        }

        this.assertProcessInstancePending(processInstance.toString(), BPMN_PROCESS_DEFINITION_KEY);
        this.waitUserTaskAssignment(processInstance.toString(), USER_TASK_1, BPMN_USER);

        // -----------------------------------------------------------------------------------------------------
        // Complete the 1st user task:
        // - After the user task completion, the service 'Archive' is called by a sub-process
        // -----------------------------------------------------------------------------------------------------

        {
            final Unlock unlock = new Unlock();
            unlock.setProcessInstanceId(processInstance.toString());
            unlock.setUnlocker(BPMN_USER);

            final RequestToProviderMessage request = new RequestToProviderMessage(COMPONENT_UNDER_TEST, PLACEHOLDERS_SU,
                    OPERATION_UNLOCK, AbsItfOperation.MEPPatternConstants.IN_OUT.value(), toByteArray(unlock));

            COMPONENT.sendAndCheckResponseAndSendStatus(request, new ServiceProviderImplementation() {
                private MessageExchange msgExchange;

                @Override
                public Message provides(final RequestMessage requestMsg) throws Exception {

                    this.msgExchange = requestMsg.getMessageExchange();
                    assertNotNull(this.msgExchange);
                    assertEquals(ARCHIVE_INTERFACE, this.msgExchange.getInterfaceName());
                    assertEquals(ARCHIVE_SERVICE, this.msgExchange.getService());
                    assertNotNull(this.msgExchange.getEndpoint());
                    assertEquals(ARCHIVE_ENDPOINT, this.msgExchange.getEndpoint().getEndpointName());
                    assertEquals(ARCHIVER_OPERATION, this.msgExchange.getOperation());
                    assertEquals(this.msgExchange.getStatus(), ExchangeStatus.ACTIVE);
                    final Object requestObj = UNMARSHALLER.unmarshal(requestMsg.getPayload());
                    assertTrue(requestObj instanceof Archiver);
                    assertEquals(PLACEHOLDER_VALUE_2, ((Archiver) requestObj).getItem2());

                    // Returns the reply of the service provider to the Flowable service task
                    return new ResponseToConsumerMessage(requestMsg, toByteArray(new ArchiverResponse()));
                }

                @Override
                public void handleStatus(final StatusMessage statusDoneMsg) throws Exception {
                    // Assert the status DONE on the message exchange
                    assertNotNull(statusDoneMsg);
                    // It's the same message exchange instance
                    assertSame(statusDoneMsg.getMessageExchange(), this.msgExchange);
                    assertEquals(statusDoneMsg.getMessageExchange().getStatus(), ExchangeStatus.DONE);
                }
            }, new MessageChecks() {

                @Override
                public void checks(final Message message) throws Exception {
                    // Check the reply
                    final Source fault = message.getFault();
                    assertNull("Unexpected fault", (fault == null ? null : SourceHelper.toString(fault)));
                    assertNotNull("No XML payload in response", message.getPayload());
                    final Object responseObj = UNMARSHALLER.unmarshal(message.getPayload());
                    assertTrue(responseObj instanceof UnlockAck);
                    final UnlockAck response = (UnlockAck) responseObj;
                    assertEquals(processInstance.toString(), response.getAck());
                }
            }, ExchangeStatus.DONE);
        }

        this.waitEndOfProcessInstance(processInstance.toString());

    }
}
