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

import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.net.URL;

import javax.xml.namespace.QName;

import org.junit.jupiter.api.BeforeAll;
import org.ow2.petals.component.framework.jbidescriptor.generated.MEPType;
import org.ow2.petals.component.framework.junit.impl.ConsumesServiceConfiguration;
import org.ow2.petals.component.framework.junit.impl.ProvidesServiceConfiguration;
import org.ow2.petals.component.framework.junit.impl.ServiceConfiguration;
import org.ow2.petals.component.framework.junit.rule.ServiceConfigurationFactory;

import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.JAXBException;
import jakarta.xml.bind.Marshaller;

/**
 * Abstract class for unit tests about process invoking InOnly or RobustInOnly services
 * 
 * @author Christophe DENEUX - Linagora
 */
public abstract class ProcessWithRobustifiedInOnlyConsumerTestEnvironment extends AbstractTestEnvironment {

    protected static final String ROBUSTIFIED_INONLY_SU = "robustified-in-only";

    protected static final String ROBUSTIFIED_INONLY_SU_HOME = "su/robustified-in-only/";

    private static final String ROBUSTIFIED_INONLY_NAMESPACE = "http://petals.ow2.org/se-flowable/unit-test/robustified-in-only";

    protected static final QName ROBUSTIFIED_INONLY_INTERFACE = new QName(ROBUSTIFIED_INONLY_NAMESPACE,
            "robustified-in-only");

    protected static final QName ROBUSTIFIED_INONLY_SERVICE = new QName(ROBUSTIFIED_INONLY_NAMESPACE,
            "robustified-in-only-service");

    protected static final String ROBUSTIFIED_INONLY_ENDPOINT = "edpRobustifiedInOnly";

    protected static final QName OPERATION_START = new QName(ROBUSTIFIED_INONLY_NAMESPACE, "start");

    private static final String ARCHIVE_NAMESPACE = "http://petals.ow2.org/se-flowable/unit-test/robustified-in-only/archivageService";

    protected static final QName ARCHIVE_INTERFACE = new QName(ARCHIVE_NAMESPACE, "archiver");

    protected static final QName ARCHIVE_SERVICE = new QName(ARCHIVE_NAMESPACE, "archiverService");

    protected static final String ARCHIVE_ENDPOINT = "archiveEndpointName";

    protected static final QName ARCHIVER_OPERATION = new QName(ARCHIVE_NAMESPACE, "archiver");

    protected static final String BPMN_PROCESS_DEFINITION_KEY = "robustified-in-only";

    @BeforeAll
    private static void initJaxBTooling() throws JAXBException {
        final JAXBContext context = JAXBContext.newInstance(
                org.ow2.petals.se_flowable.unit_test.robustified_in_only.ObjectFactory.class,
                org.ow2.petals.se_flowable.unit_test.robustified_in_only.archivageservice.ObjectFactory.class);
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
        COMPONENT_UNDER_TEST.registerServiceToDeploy(ROBUSTIFIED_INONLY_SU, new ServiceConfigurationFactory() {
            @Override
            public ServiceConfiguration create() {

                final URL wsdlUrl = Thread.currentThread().getContextClassLoader()
                        .getResource(ROBUSTIFIED_INONLY_SU_HOME + "robustified-in-only.wsdl");
                assertNotNull(wsdlUrl, "WSDL not found");
                final ProvidesServiceConfiguration serviceConfiguration = new ProvidesServiceConfiguration(
                        ROBUSTIFIED_INONLY_INTERFACE, ROBUSTIFIED_INONLY_SERVICE, ROBUSTIFIED_INONLY_ENDPOINT, wsdlUrl);

                final URL startResponseXslUrl = Thread.currentThread().getContextClassLoader()
                        .getResource(ROBUSTIFIED_INONLY_SU_HOME + "startResponse.xsl");
                assertNotNull(startResponseXslUrl, "Output XSL 'startResponse.xsl' not found");
                serviceConfiguration.addResource(startResponseXslUrl);

                final URL bpmnUrl = Thread.currentThread().getContextClassLoader()
                        .getResource(ROBUSTIFIED_INONLY_SU_HOME + "robustified-in-only.bpmn");
                assertNotNull(bpmnUrl, "BPMN file not found");
                serviceConfiguration.addResource(bpmnUrl);

                final URL archivageServiceWsdlUrl = Thread.currentThread().getContextClassLoader()
                        .getResource(ROBUSTIFIED_INONLY_SU_HOME + "archivageService.wsdl");
                assertNotNull(archivageServiceWsdlUrl, "archivageService WSDL not found");
                serviceConfiguration.addResource(archivageServiceWsdlUrl);

                serviceConfiguration.setServicesSectionParameter(
                        new QName(FlowableSEConstants.NAMESPACE_SU, "process_file"), "robustified-in-only.bpmn");
                serviceConfiguration.setServicesSectionParameter(new QName(FlowableSEConstants.NAMESPACE_SU, "version"),
                        "1");

                // We force the MEP to use to invoke archiving service with MEP 'RobustInOnly'
                final ConsumesServiceConfiguration serviceConsumerCfg = new ConsumesServiceConfiguration(
                        ARCHIVE_INTERFACE, ARCHIVE_SERVICE, ARCHIVE_ENDPOINT);
                serviceConsumerCfg.setMEP(MEPType.ROBUST_IN_ONLY);
                serviceConsumerCfg.setOperation(ARCHIVER_OPERATION);
                serviceConfiguration.addServiceConfigurationDependency(serviceConsumerCfg);

                return serviceConfiguration;
            }
        }).registerExternalServiceProvider(ARCHIVE_ENDPOINT, ARCHIVE_SERVICE, ARCHIVE_INTERFACE)
                .postInitComponentUnderTest();
    }
}
