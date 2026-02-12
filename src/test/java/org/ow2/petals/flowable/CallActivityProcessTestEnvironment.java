/**
 * Copyright (c) 2014-2026 Linagora
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

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_PROCESSINSTANCES_PORT_TYPE;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_PROCESSINSTANCES_SERVICE;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_TASK_PORT_TYPE;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_TASK_SERVICE;

import java.net.URL;

import javax.xml.namespace.QName;

import org.junit.jupiter.api.BeforeAll;
import org.ow2.petals.component.framework.junit.impl.ConsumesServiceConfiguration;
import org.ow2.petals.component.framework.junit.impl.ProvidesServiceConfiguration;
import org.ow2.petals.component.framework.junit.impl.ServiceConfiguration;
import org.ow2.petals.component.framework.junit.rule.NativeServiceConfigurationFactory;
import org.ow2.petals.component.framework.junit.rule.ServiceConfigurationFactory;

import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.JAXBException;
import jakarta.xml.bind.Marshaller;

/**
 * Abstract class for unit tests about request processing
 * 
 * @author Christophe DENEUX - Linagora
 */
public abstract class CallActivityProcessTestEnvironment extends AbstractTestEnvironment {

    protected static final String CALL_ACTIVITY_SU = "call-activity-su";

    protected static final String CALL_ACTIVITY_SU_HOME = "su/call-activity/";

    protected static final String CALL_ACTIVITY_PROVIDER_SU = "call-activity-provider-su";

    private static final String CALL_ACTIVITY_NAMESPACE = "http://petals.ow2.org/se-flowable/unit-test/call-activity/level1";

    protected static final QName CALL_ACTIVITY_INTERFACE = new QName(CALL_ACTIVITY_NAMESPACE, "call-activity");

    protected static final QName CALL_ACTIVITY_SERVICE = new QName(CALL_ACTIVITY_NAMESPACE, "call-activity-service");

    protected static final String CALL_ACTIVITY_ENDPOINT = "edpCallActivity";

    protected static final QName OPERATION_START = new QName(CALL_ACTIVITY_NAMESPACE, "start");

    protected static final QName OPERATION_UNLOCK = new QName(CALL_ACTIVITY_NAMESPACE, "unlock");

    protected static final String BPMN_PROCESS_DEFINITION_KEY = "processLevel1";

    protected static final String BPMN_USER = "kermit";

    private static final String ARCHIVE_NAMESPACE = "http://petals.ow2.org/se-flowable/unit-test/call-activity/archivageService";

    protected static final QName ARCHIVE_INTERFACE = new QName(ARCHIVE_NAMESPACE, "archiver");

    protected static final QName ARCHIVE_SERVICE = new QName(ARCHIVE_NAMESPACE, "archiverService");

    protected static final String ARCHIVE_ENDPOINT = "archiveEndpointName";

    protected static final QName ARCHIVER_OPERATION = new QName(ARCHIVE_NAMESPACE, "archiver");

    private static final String CORE_SVC_NAMESPACE = "http://petals.ow2.org/se-flowable/unit-test/call-activity/coreService";

    protected static final QName CORE_SVC_INTERFACE = new QName(CORE_SVC_NAMESPACE, "core");

    protected static final QName CORE_SVC_SERVICE = new QName(CORE_SVC_NAMESPACE, "coreService");

    protected static final String CORE_SVC_ENDPOINT = "coreServiceEndpointName";

    protected static final QName CORE_SVC_OPERATION = new QName(CORE_SVC_NAMESPACE, "execute");

    @BeforeAll
    private static void initJaxBTooling() throws JAXBException {
        final JAXBContext context = JAXBContext.newInstance(
                org.ow2.petals.se_flowable.unit_test.call_activity.archivageservice.ObjectFactory.class,
                org.ow2.petals.se_flowable.unit_test.call_activity.level1.ObjectFactory.class,
                org.ow2.petals.se_flowable.unit_test.call_activity.coreservice.ObjectFactory.class,
                org.ow2.petals.components.flowable.generic._1.ObjectFactory.class);
        UNMARSHALLER = context.createUnmarshaller();
        MARSHALLER = context.createMarshaller();
        MARSHALLER.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
    }

    @BeforeAll
    private static void completesTestEnvConfiguration() throws Exception {
        FLOWABLE_CLIENT.start();
        completesComponentUnderTestConfiguration();
    }

    private static void completesComponentUnderTestConfiguration() throws Exception {
        COMPONENT_UNDER_TEST.registerServiceToDeploy(CALL_ACTIVITY_SU, new ServiceConfigurationFactory() {
            @Override
            public ServiceConfiguration create() {

                final ServiceConfiguration serviceConfiguration = new ServiceConfiguration();

                final URL bpmnUrl_3 = Thread.currentThread().getContextClassLoader()
                        .getResource(CALL_ACTIVITY_SU_HOME + "Process-Level-3.bpmn");
                assertNotNull(bpmnUrl_3, "BPMN file not found");
                serviceConfiguration.addResource(bpmnUrl_3);

                final URL coreServiceWsdlUrl = Thread.currentThread().getContextClassLoader()
                        .getResource(CALL_ACTIVITY_SU_HOME + "coreService.wsdl");
                assertNotNull(coreServiceWsdlUrl, "coreService WSDL not found");
                serviceConfiguration.addResource(coreServiceWsdlUrl);

                serviceConfiguration.setServicesSectionParameter(
                        new QName(FlowableSEConstants.NAMESPACE_SU, "process_file"), "Process-Level-3.bpmn");
                serviceConfiguration.setServicesSectionParameter(new QName(FlowableSEConstants.NAMESPACE_SU, "version"),
                        "1");

                // Consume service 'core'
                final ConsumesServiceConfiguration consumeServiceConfiguration = new ConsumesServiceConfiguration(
                        CORE_SVC_INTERFACE, CORE_SVC_SERVICE, CORE_SVC_ENDPOINT);
                serviceConfiguration.addServiceConfigurationDependency(consumeServiceConfiguration);

                return serviceConfiguration;
            }
        }).registerServiceToDeploy(CALL_ACTIVITY_PROVIDER_SU, new ServiceConfigurationFactory() {
            @Override
            public ServiceConfiguration create() {

                final URL wsdlUrl = Thread.currentThread().getContextClassLoader()
                        .getResource(CALL_ACTIVITY_SU_HOME + "call-activity.wsdl");
                assertNotNull(wsdlUrl, "WSDL not found");
                final ProvidesServiceConfiguration serviceConfiguration = new ProvidesServiceConfiguration(
                        CALL_ACTIVITY_INTERFACE, CALL_ACTIVITY_SERVICE, CALL_ACTIVITY_ENDPOINT, wsdlUrl);

                final URL startResponseXslUrl = Thread.currentThread().getContextClassLoader()
                        .getResource(CALL_ACTIVITY_SU_HOME + "startResponse.xsl");
                assertNotNull(startResponseXslUrl, "Output XSL 'startResponse.xsl' not found");
                serviceConfiguration.addResource(startResponseXslUrl);

                final URL unlockResponseXslUrl = Thread.currentThread().getContextClassLoader()
                        .getResource(CALL_ACTIVITY_SU_HOME + "unlockAckResponse.xsl");
                assertNotNull(unlockResponseXslUrl, "Output XSL 'unlockAckResponse.xsl' not found");
                serviceConfiguration.addResource(unlockResponseXslUrl);

                final URL alreadyUnlockedXslUrl = Thread.currentThread().getContextClassLoader()
                        .getResource(CALL_ACTIVITY_SU_HOME + "alreadyUnlocked.xsl");
                assertNotNull(alreadyUnlockedXslUrl, "Output XSL 'alreadyUnlocked.xsl' not found");
                serviceConfiguration.addResource(alreadyUnlockedXslUrl);

                final URL instanceUnknownXslUrl = Thread.currentThread().getContextClassLoader()
                        .getResource(CALL_ACTIVITY_SU_HOME + "instanceUnknown.xsl");
                assertNotNull(instanceUnknownXslUrl, "Output XSL 'instanceUnknown.xsl' not found");
                serviceConfiguration.addResource(instanceUnknownXslUrl);

                final URL bpmnUrl_1 = Thread.currentThread().getContextClassLoader()
                        .getResource(CALL_ACTIVITY_SU_HOME + "Process-Level-1.bpmn");
                assertNotNull(bpmnUrl_1, "BPMN file not found");
                serviceConfiguration.addResource(bpmnUrl_1);

                final URL archivageServiceWsdlUrl = Thread.currentThread().getContextClassLoader()
                        .getResource(CALL_ACTIVITY_SU_HOME + "archivageService.wsdl");
                assertNotNull(archivageServiceWsdlUrl, "archivageService WSDL not found");
                serviceConfiguration.addResource(archivageServiceWsdlUrl);

                serviceConfiguration.setServicesSectionParameter(
                        new QName(FlowableSEConstants.NAMESPACE_SU, "process_file1"), "Process-Level-1.bpmn");
                serviceConfiguration
                        .setServicesSectionParameter(new QName(FlowableSEConstants.NAMESPACE_SU, "version1"), "1");

                final URL bpmnUrl_2 = Thread.currentThread().getContextClassLoader()
                        .getResource(CALL_ACTIVITY_SU_HOME + "Process-Level-2.bpmn");
                assertNotNull(bpmnUrl_2, "BPMN file not found");
                serviceConfiguration.addResource(bpmnUrl_2);

                serviceConfiguration.setServicesSectionParameter(
                        new QName(FlowableSEConstants.NAMESPACE_SU, "process_file2"), "Process-Level-2.bpmn");
                serviceConfiguration
                        .setServicesSectionParameter(new QName(FlowableSEConstants.NAMESPACE_SU, "version2"), "1");

                // Consume service 'archiver'
                final ConsumesServiceConfiguration consumeServiceConfiguration = new ConsumesServiceConfiguration(
                        ARCHIVE_INTERFACE, ARCHIVE_SERVICE, ARCHIVE_ENDPOINT);
                serviceConfiguration.addServiceConfigurationDependency(consumeServiceConfiguration);

                return serviceConfiguration;
            }
        }).registerNativeServiceToDeploy(NATIVE_TASKS_SVC_CFG, new NativeServiceConfigurationFactory() {

            @Override
            public ServiceConfiguration create(final String nativeEndpointName) {

                final URL nativeServiceWsdlUrl = Thread.currentThread().getContextClassLoader()
                        .getResource("component.wsdl");
                assertNotNull(nativeServiceWsdlUrl, "Integration servce WSDl not found");
                return new ProvidesServiceConfiguration(ITG_TASK_PORT_TYPE, ITG_TASK_SERVICE, nativeEndpointName,
                        nativeServiceWsdlUrl);
            }

            @Override
            public QName getNativeService() {
                return ITG_TASK_SERVICE;
            }
        }).registerNativeServiceToDeploy(NATIVE_PROCESSINSTANCES_SVC_CFG, new NativeServiceConfigurationFactory() {

            @Override
            public ServiceConfiguration create(final String nativeEndpointName) {

                final URL nativeServiceWsdlUrl = Thread.currentThread().getContextClassLoader()
                        .getResource("component.wsdl");
                assertNotNull(nativeServiceWsdlUrl, "Integration servce WSDl not found");
                return new ProvidesServiceConfiguration(ITG_PROCESSINSTANCES_PORT_TYPE, ITG_PROCESSINSTANCES_SERVICE,
                        nativeEndpointName, nativeServiceWsdlUrl);
            }

            @Override
            public QName getNativeService() {
                return ITG_PROCESSINSTANCES_SERVICE;
            }
        }).registerExternalServiceProvider(ARCHIVE_ENDPOINT, ARCHIVE_SERVICE, ARCHIVE_INTERFACE)
                .registerExternalServiceProvider(CORE_SVC_ENDPOINT, CORE_SVC_SERVICE, CORE_SVC_INTERFACE)
                .postInitComponentUnderTest();
    }
}
