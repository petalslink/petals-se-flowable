/**
 * Copyright (c) 2017-2024 Linagora
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
import org.ow2.easywsdl.wsdl.api.abstractItf.AbsItfOperation.MEPPatternConstants;
import org.ow2.petals.component.framework.junit.Message;
import org.ow2.petals.component.framework.junit.RequestMessage;
import org.ow2.petals.component.framework.junit.helpers.MessageChecks;
import org.ow2.petals.component.framework.junit.helpers.ServiceProviderImplementation;
import org.ow2.petals.component.framework.junit.impl.message.RequestToProviderMessage;
import org.ow2.petals.component.framework.junit.impl.message.StatusToConsumerMessage;
import org.ow2.petals.se_flowable.unit_test.in_only.Start;
import org.ow2.petals.se_flowable.unit_test.in_only.StartResponse;
import org.ow2.petals.se_flowable.unit_test.in_only.archivageservice.Archiver;

import com.ebmwebsourcing.easycommons.xml.SourceHelper;

/**
 * Unit tests about request processing of BPMN services with MEP 'InOnly'
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class ProcessWithInOnlyConsumerTest extends ProcessWithInOnlyConsumerTestEnvironment {

    /**
     * <p>
     * Check the service invocation with MEP InOnly returning an acknowledge
     * </p>
     * <p>
     * Expected results:
     * </p>
     * <ul>
     * <li>the process instance is correctly created in the Flowable engine,</li>
     * <li>the service is correctly invoked,</li>
     * <li>the process instance is automatically completed when all is correctly done.</li>
     * </ul>
     */
    @Test
    public void nominal() throws Exception {

        final StringBuilder processInstanceId = new StringBuilder();
        final Start start = new Start();

        final RequestToProviderMessage request = new RequestToProviderMessage(COMPONENT_UNDER_TEST, INONLY_SU,
                OPERATION_START, AbsItfOperation.MEPPatternConstants.IN_OUT.value(), toByteArray(start));

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
                assertEquals(ExchangeStatus.ACTIVE, this.msgExchange.getStatus());
                assertEquals(MEPPatternConstants.IN_ONLY,
                        MEPPatternConstants.fromURI(this.msgExchange.getPattern()));
                final Object requestObj = UNMARSHALLER.unmarshal(requestMsg.getPayload());
                assertTrue(requestObj instanceof Archiver);

                // Returns the reply of the service provider to the Flowable service task
                return new StatusToConsumerMessage(requestMsg, ExchangeStatus.DONE);
            }

            @Override
            public boolean statusExpected() {
                return false;
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
                processInstanceId.append(response.getCaseFileNumber());
            }
        }, ExchangeStatus.DONE);

        this.waitEndOfProcessInstance(processInstanceId.toString());

    }
}
